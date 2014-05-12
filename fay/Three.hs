{-# LANGUAGE OverloadedStrings #-}
module Three where

import Fay.Text as T
import FFI
import Prelude
import Animation
import JQuery

data Camera
data Scene
data Geometry
data Material
data Materials
data Mesh
data Renderer
data JSONLoader
data Light
type GeoCallback = Geometry -> Fay ()

data Vector3 = Vector3 { x::Double, y::Double, z::Double }

mkVector3 :: Double -> Double -> Double -> Fay Vector3
mkVector3 = ffi "new THREE.Vector3(%1,%2,%3)"

eventClientX :: Event -> Fay Double
eventClientX = ffi "%1.clientX"

eventClientY :: Event -> Fay Double
eventClientY = ffi "%1.clientY"

newJSONLoader :: Fay JSONLoader
newJSONLoader = ffi "new THREE.JSONLoader()"

loadModel :: Text -> (Geometry -> Materials -> Fay ()) -> JSONLoader -> Fay ()
loadModel = ffi "%3.load(%1,%2)"

getModelUrl :: Fay Text
getModelUrl = ffi "modelurl"

createMeshInScene :: Geometry -> Materials -> Vector3 -> Scene -> Fay ()
createMeshInScene geometry materials pos scene = do
    material <- newMeshFaceMaterial materials
    mesh <- newMesh geometry material
    setMeshPosition pos mesh
    setMeshScale 1 1 1 mesh
    addMesh mesh scene
    return ()

alert :: Text -> Fay ()
alert = ffi "alert(%1)"

addBoxToScene :: Double -> Double -> Double -> Scene -> Fay Mesh
addBoxToScene dx dy dz scene = do
    geometry <- newBox dx dy dz
    material <- newMaterial
    mesh <- newMesh geometry material
    addMesh mesh scene
    return mesh

setMeshPosition :: Vector3 -> Mesh -> Fay ()
setMeshPosition = ffi "%2['position']=%1"

logF:: f -> Fay ()
logF = ffi "console.log(%1)"

setMeshScale :: Double -> Double -> Double -> Mesh -> Fay ()
setMeshScale = ffi "%4.scale.set(%1, %2, %3)"

newMeshFaceMaterial :: Materials -> Fay Material
newMeshFaceMaterial = ffi "new THREE.MeshFaceMaterial(%1)"

newAmbientLight :: Double -> Fay Light
newAmbientLight = ffi "new THREE.AmbientLight(%1)"

mkDirectionalLight :: Vector3 -> Double -> Fay Light
mkDirectionalLight dir color = do
    light <- newDirectionalLight color
    setLightDirection dir light
    return light

newDirectionalLight :: Double -> Fay Light
newDirectionalLight = ffi "new THREE.DirectionalLight(%1)"

setLightDirection :: Vector3 -> Light -> Fay ()
setLightDirection = ffi "%2.position.set(%1['x'],%1['y'],%1['z']).normalize()"

addLight :: Light -> Scene -> Fay ()
addLight = ffi "%2.add(%1)"

addLightToScene :: Scene -> Light -> Fay ()
addLightToScene = ffi "%1.add(%2)"

windowInnerWidth :: Fay Double
windowInnerWidth = ffi "window.innerWidth"

windowInnerHeight :: Fay Double
windowInnerHeight = ffi "window.innerHeight"

newCamera :: Double -> Double -> Double -> Fay Camera
newCamera = ffi "new THREE.PerspectiveCamera( 60, %1, %2, %3)"

setCameraAspect :: Double -> Camera -> Fay ()
setCameraAspect = ffi "%2.aspect = %1"

setCameraPos :: Vector3 -> Camera -> Fay Camera
setCameraPos = ffi "%2['position']=%1"

setCameraPosZ :: Double -> Camera -> Fay Camera
setCameraPosZ = ffi "%2['position']['z']=(%1)"

lookAt :: Vector3 -> Camera -> Fay ()
lookAt = ffi "%2.lookAt(%1)"

getScenePosition :: Scene -> Fay Vector3
getScenePosition = ffi "%1['position']"

lookAtScene :: Scene -> Camera -> Fay ()
lookAtScene scene camera = do
    at <- getScenePosition scene
    lookAt at camera

updateProjection :: Camera -> Fay ()
updateProjection = ffi "%1['updateProjectionMatrix']()"

modifyCameraPosition :: (Double -> Double) -> (Double -> Double) -> Camera -> Fay ()
modifyCameraPosition = ffi "(function (fx, fy, cam) { cam.position.x += fx(cam.position.x); cam.position.y += fy(cam.position.y)})(%1,%2,%3)"

newScene :: Fay Scene
newScene = ffi "new THREE.Scene()"

newBox :: Double -> Double -> Double -> Fay Geometry
newBox = ffi "new THREE.BoxGeometry( %1, %2, %3 )"

newMaterial :: Fay Material
newMaterial = ffi "new THREE.MeshBasicMaterial( { color: 0x6699dd } )"

newMesh :: Geometry -> Material -> Fay Mesh
newMesh = ffi "new THREE.Mesh((%1),(%2))"

addMesh ::  Mesh -> Scene -> Fay ()
addMesh = ffi "%2['add'](%1)"

newRenderer :: Fay Renderer
newRenderer = ffi "new THREE.WebGLRenderer()"

setRendererSize :: Double -> Double -> Renderer -> Fay ()
setRendererSize = ffi "%3['setSize'](%1,%2)"

setClearColor :: Double -> Renderer -> Fay ()
setClearColor = ffi "%2.setClearColor(%1)"

domElement :: Renderer -> Fay Element
domElement = ffi "%1['domElement']"

addElementToBody :: Element -> Fay ()
addElementToBody = ffi "document.body.appendChild(%1)"

appendAsChild :: JQuery -> Element -> Fay ()
appendAsChild = ffi "%1.append(%2)"

rotateMesh :: Mesh -> Fay ()
rotateMesh = ffi "(function (m) { m.rotation.x += 0.01; m.rotation.y += 0.02; })(%1)"

render3d :: Scene -> Camera -> Renderer -> Fay ()
render3d = ffi "%3['render'](%1,%2)"
