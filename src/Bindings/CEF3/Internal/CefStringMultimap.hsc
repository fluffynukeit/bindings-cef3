{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "include/internal/cef_string_multimap.h"
#include "include/internal/cef_export.h"
module Bindings.CEF3.Internal.CefStringMultimap where
import Foreign.Ptr
#strict_import

import Bindings.CEF3.Internal.CefString
{- typedef void * cef_string_multimap_t; -}
#synonym_t cef_string_multimap_t, Ptr ()

#ccall cef_string_multimap_alloc , IO <cef_string_multimap_t>
#ccall cef_string_multimap_size , <cef_string_multimap_t> -> IO CInt
#ccall cef_string_multimap_find_count , <cef_string_multimap_t> -> Ptr <cef_string_utf16_t> -> IO CInt
#ccall cef_string_multimap_enumerate , <cef_string_multimap_t> -> Ptr <cef_string_utf16_t> -> CInt -> Ptr <cef_string_utf16_t> -> IO CInt
#ccall cef_string_multimap_key , <cef_string_multimap_t> -> CInt -> Ptr <cef_string_utf16_t> -> IO CInt
#ccall cef_string_multimap_value , <cef_string_multimap_t> -> CInt -> Ptr <cef_string_utf16_t> -> IO CInt
#ccall cef_string_multimap_append , <cef_string_multimap_t> -> Ptr <cef_string_utf16_t> -> Ptr <cef_string_utf16_t> -> IO CInt
#ccall cef_string_multimap_clear , <cef_string_multimap_t> -> IO ()
#ccall cef_string_multimap_free , <cef_string_multimap_t> -> IO ()
