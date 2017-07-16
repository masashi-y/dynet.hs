
module DyNet.IO (
    TextFileSaver,
    TextFileLoader,
    createSaver,
    createSaver',
    saveModel,
    saveParameter,
    saveLookupParameter,
    saveModel',
    saveParameter',
    saveLookupParameter',
    createLoader,
    populateModel,
    populateParameter,
    populateLookupParameter,
    populateModel',
    populateParameter',
    populateLookupParameter',
    loadParam,
    loadLookupParam
) where

import DyNet.Internal.IO

createSaver' f = createSaver f False
saveModel' s m = saveModel s m ""
saveParameter' s m = saveParameter s m ""
saveLookupParameter' s m = saveLookupParameter s m ""
populateModel' s m = populateModel s m ""
populateParameter' s m = populateParameter s m ""
populateLookupParameter' s m = populateLookupParameter s m ""
