
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

{#fun save_ParameterCollection as saveModel
    {`TextFileSaver', `Model', `String'} -> `()' #}

{#fun save_Parameter as ^
    {`TextFileSaver', `Parameter', `String'} -> `()' #}

{#fun save_LookupParameter as ^
    {`TextFileSaver', `LookupParameter', `String'} -> `()' #}




{#fun init_TextFileLoader as createLoader
    {+S, `String'} -> `TextFileLoader' #}

{#fun pure size_of_TextFileLoader as ^ {} -> `Int' #}

{#fun populate_ParameterCollection as populateModel
    {`TextFileLoader', `Model', `String'} -> `()' #}

{#fun populate_Parameter as ^
    {`TextFileLoader', `Parameter', `String'} -> `()' #}

{#fun populate_LookupParameter as ^
    {`TextFileLoader', `LookupParameter', `String'} -> `()' #}

{#fun load_param as ^
    {`TextFileLoader', +S, `Model', `String'} -> `Parameter' #}

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

