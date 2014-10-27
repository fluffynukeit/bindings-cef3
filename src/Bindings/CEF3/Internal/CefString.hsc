{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "include/internal/cef_string.h"
module Bindings.CEF3.Internal.CefString 
( module Bindings.CEF3.Internal.CefStringTypes
, module Bindings.CEF3.Internal.CefString
) where
import Foreign.Ptr
#strict_import

import Bindings.CEF3.Internal.CefStringTypes

{- typedef char16 cef_char_t; -}
#synonym_t cef_char_t , CUShort
{- typedef cef_string_userfree_utf16_t cef_string_userfree_t; -}
#synonym_t cef_string_userfree_t , <cef_string_userfree_utf16_t>
{- typedef cef_string_utf16_t cef_string_t; -}
#synonym_t cef_string_t , <cef_string_utf16_t>

c'cef_string_set =            c'cef_string_utf16_set
c'cef_string_copy =           c'cef_string_utf16_copy
c'cef_string_clear =          c'cef_string_utf16_clear
c'cef_string_userfree_alloc = c'cef_string_userfree_utf16_alloc
c'cef_string_userfree_free =  c'cef_string_userfree_utf16_free
c'cef_string_from_ascii =     c'cef_string_ascii_to_utf16
c'cef_string_to_utf8 =        c'cef_string_utf16_to_utf8
c'cef_string_from_utf8 =      c'cef_string_utf8_to_utf16
c'cef_string_to_utf16 =       c'cef_string_utf16_copy
c'cef_string_from_utf16 =     c'cef_string_utf16_copy
c'cef_string_to_wide =        c'cef_string_utf16_to_wide
c'cef_string_from_wide =      c'cef_string_wide_to_utf16
