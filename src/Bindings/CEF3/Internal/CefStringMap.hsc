{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "include/internal/cef_string_map.h"
#include "include/internal/cef_export.h"
module Bindings.CEF3.Internal.CefStringMap where
import Foreign.Ptr
#strict_import

import Bindings.CEF3.Internal.CefString
{- typedef void * cef_string_map_t; -}
#synonym_t cef_string_map_t, Ptr ()

#ccall cef_string_map_alloc , IO <cef_string_map_t>
#ccall cef_string_map_size , <cef_string_map_t> -> IO CInt
#ccall cef_string_map_find , <cef_string_map_t> -> Ptr <cef_string_utf16_t> -> Ptr <cef_string_utf16_t> -> IO CInt
#ccall cef_string_map_key , <cef_string_map_t> -> CInt -> Ptr <cef_string_utf16_t> -> IO CInt
#ccall cef_string_map_value , <cef_string_map_t> -> CInt -> Ptr <cef_string_utf16_t> -> IO CInt
#ccall cef_string_map_append , <cef_string_map_t> -> Ptr <cef_string_utf16_t> -> Ptr <cef_string_utf16_t> -> IO CInt
#ccall cef_string_map_clear , <cef_string_map_t> -> IO ()
#ccall cef_string_map_free , <cef_string_map_t> -> IO ()
