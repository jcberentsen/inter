{-# LANGUAGE OverloadedStrings #-}
module Webgl where

import Fay.Text as T
import FFI
import Prelude
import Animation
import JQuery
import Three
import FayRef

init3d :: JQuery -> Fay (Scene, Camera, Renderer)
init3d div = do
    viewWidth <- windowInnerWidth
    viewHeight <- windowInnerHeight
    let aspect = viewWidth / viewHeight
    let near :: Double
        near = 0.1
        far :: Double
        far = 50.0
    camera <- newCamera aspect near far
    updateProjection camera
    setCameraPos (Vector3 0 0 (-5)) camera -- meters
    lookAt (Vector3 0 0 0) camera
    updateProjection camera

    scene <- newScene

    renderer <- newRenderer
    setRendererSize viewWidth viewHeight renderer
    setClearColor 0x010101 renderer

    ambient <- newAmbientLight 0x000000
    addLight ambient scene
    mkDirectionalLight (Vector3 (-1) 2 (-3)) 0xccccdd >>= addLightToScene scene
    mkDirectionalLight (Vector3 3 1 (-2)) 0x777777 >>= addLightToScene scene

    canvas <- domElement renderer
    appendAsChild div canvas

    return (scene, camera, renderer)

rendererOnResize :: Camera -> Renderer -> Event -> Fay ()
rendererOnResize camera renderer e = do
    viewWidth <- windowInnerWidth
    viewHeight <- windowInnerHeight
    let aspect = (fromIntegral viewWidth) / (fromIntegral viewHeight)
    setCameraAspect aspect camera
    updateProjection camera
    setRendererSize viewWidth viewHeight renderer

onCanvasMouseMove :: FayRef (Double,Double) -> Event ->  Fay ()
onCanvasMouseMove mouseRef e = do
    width <-  windowInnerWidth
    height <-  windowInnerHeight
    x <- eventClientX e
    y <- eventClientY e
    writeFayRef mouseRef ((x - (width / 2.0)), (y - (height / 2.0)))

animate3d :: Scene -> Camera -> Renderer -> FayRef (Double,Double) -> Fay ()
animate3d scene camera renderer mouseRef = do
    requestAnimationFrame (animate3d scene camera renderer mouseRef)

    (mouseX, mouseY) <- readFayRef mouseRef

    modifyCameraPosition (\x -> (mouseX/100 - x) / 20.0) (\y -> (0 - mouseY/50 - y) / 20.0) camera

    lookAtScene scene camera
    updateProjection camera

    --rotateMesh mesh
    render3d scene camera renderer

loadGeometry :: Scene -> Fay ()
loadGeometry scene = do
    loader <- newJSONLoader
    let model_pos = Vector3 (-5000) (-5000) 0
    let model_callback = \geometry materials -> createMeshInScene geometry materials model_pos scene >> return ()
    model_url <- getModelUrl
    loadModel model_url model_callback loader

loadColladaMesh :: Scene -> Fay ()
loadColladaMesh scene = do
    loader <- newColladaLoader
    model_url <- getColladaModelUrl
    let onLoaded = \collada -> do
            logF collada
            dae <- getColladaScene collada
            let s :: Double
                s = 0.0254
            logF dae
            setMeshScale s s s dae
            updateMeshMatrix dae
            addMesh dae scene
            return ()

        onProgress = \a -> logF a
    loadCollada model_url loader onLoaded onProgress

data ColladaLoader
data Collada

newColladaLoader :: Fay ColladaLoader
newColladaLoader = ffi "new THREE.ColladaLoader()"

type OnColladaLoaded = Collada -> Fay ()
type OnColladaProgress a = a -> Fay ()

loadCollada :: Text -> ColladaLoader -> (Collada -> Fay ()) -> (a -> Fay ()) -> Fay ()
loadCollada = ffi "%2.load(%1,%3,%4)"

getColladaScene :: Collada -> Fay Mesh
getColladaScene = ffi "%1.scene"

getColladaModelUrl :: Fay Text
getColladaModelUrl = ffi "colladamodelurl"

updateMeshMatrix :: Mesh -> Fay ()
updateMeshMatrix = ffi "%1.updateMatrix()"
