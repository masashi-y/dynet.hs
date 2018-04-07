
module DyNet.Internal.IO where

{#import DyNet.Internal.Core #}

import Foreign.Storable

#include "io.h"

{#pointer *CTextFileSaver as TextFileSaver
    foreign finalizer delete_TextFileSaver newtype #}

{#pointer *CTextFileLoader as TextFileLoader
    foreign finalizer delete_TextFileLoader newtype #}


{#fun init_TextFileSaver as createSaver
    {+S, `String', `Bool'} -> `TextFileSaver' #}

{#fun pure size_of_TextFileSaver as ^ {} -> `Int' #}

{-|
[@brief@] Save ParameterCollection
[@parameters@]

    * @model@ ParameterCollection object to be saved
    * @key@ optional parameter, the name for the ParameterCollection in the saved file. This
              will default to the current name of the ParameterCollection.
[@details@]: Let's say we have a ParameterCollection named "\/pc1\/" containing parameters
           "\/pc1\/a", "\/pc1\/b", and "\/pc1\/c". This will save the parameters with the names as-is
           if `key` is not specified. If `key` is specified as "\/pc2\/", then the parameters will
           be saved as "\/pc2\/a", "\/pc2\/b", and "\/pc2\/c".
-}
{#fun save_ParameterCollection as saveModel
    {`TextFileSaver', `Model', `String'} -> `()' #}

{-|
[@brief@] Save Parameter.
[@parameters@]

    * @model@ Parameter object to be saved
    * @key@ optional parameter, the key for the parameter. This will override the Parameter's original name.
-}
{#fun save_Parameter as ^
    {`TextFileSaver', `Parameter', `String'} -> `()' #}

{-|
[@brief@] Save look parameter with key, use internal name if key is not given.
[@parameters@]

    * @model@ input LookupParameter object to be saved
    * @key@ optional parameter, the key for the parameter. This will override the Parameter's original name.
-}
{#fun save_LookupParameter as ^
    {`TextFileSaver', `LookupParameter', `String'} -> `()' #}




{#fun init_TextFileLoader as createLoader
    {+S, `String'} -> `TextFileLoader' #}

{#fun pure size_of_TextFileLoader as ^ {} -> `Int' #}

{-|
[@brief@] Populate the parameters of a ParameterCollection.
[@parameters@]

    * @model@ The ParameterCollection to be populated.
    * @key@ optional parameter, the key corresponding to the ParameterCollection

[@details@] This is the standard way to load parameters of a ParameterCollection from a
           file, and assumes that we have saved an identical ParameterCollection using
           Saver::save(parameter_collection).
           Before calling this function, we assume that the ParameterCollection has
           been fully specified, and all of its Parameters and LookupParameters have been
           created with the proper dimensions. This function will then travel through the
           file and load all parameters with names starting with prefix `key`, and populate
           the Parameters and LookupParameters one-by-one in order. When the function
           terminates, we must have populated all of the parameters in `model`. `key` is
           by default empty, so by default we will load all parameters in the file, but if
           we specify `key` we can load a subset of the parameters.

-}
{#fun populate_ParameterCollection as populateModel
    {`TextFileLoader', `Model', `String'} -> `()' #}

{-|
[@brief@] Populate independent parameter object with key.
         independent here means it has been saved without a ParameterCollection object

[@parameters@]

    * @param@ input/output parameter, the Parameter object to be populated in.
    * @key@ optional parameter, the key for loading the parameter

-}
{#fun populate_Parameter as ^
    {`TextFileLoader', `Parameter', `String'} -> `()' #}

{-|
[@brief@] Populate independent lookup parameter object with key.
         independent here means it has been saved without a LookupParameterCollection object
[@parameters@]

    * @lookup_param@ input/output parameter, the LookupParameter object to be populated in.
    * @key@ optional parameter, the key for loading the lookup parameter
-}
{#fun populate_LookupParameter as ^
    {`TextFileLoader', `LookupParameter', `String'} -> `()' #}

{-|
[@brief@] Load parameter into model with key
[@parameters@]

    * @model@ input/output parameter, the model to load parameter
    * @key@ the key for loading the parameter

[@return@] the loaded parameter
-}
{#fun load_param as ^
    {`TextFileLoader', +S, `Model', `String'} -> `Parameter' #}

{-|
[@brief@] Load lookup parameter into model with key
[@parameters@]

    * @model@ input/output parameter, the model to load the lookup parameter
    * @key@ the key for loading the lookup parameter

[@return@] the loaded lookup parameter
-}
{#fun load_lookup_param as ^
    {`TextFileLoader', +S, `Model', `String'} -> `LookupParameter' #}

instance Storable TextFileSaver where
    sizeOf _ = sizeOfTextFileSaver
    alignment _ = 4
    peek = undefined
    poke = undefined

instance Storable TextFileLoader where
    sizeOf _ = sizeOfTextFileLoader
    alignment _ = 4
    peek = undefined
    poke = undefined

