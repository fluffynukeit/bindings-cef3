{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "include/internal/cef_string_list.h"
#include "include/internal/cef_export.h"
module Bindings.CEF3.Internal.CefStringList where
import Foreign.Ptr
#strict_import

import Bindings.CEF3.Internal.CefString

{- typedef void * cef_string_list_t; -}
#synonym_t cef_string_list_t, Ptr ()

#ccall cef_string_list_alloc , IO <cef_string_list_t>
#ccall cef_string_list_size , <cef_string_list_t> -> IO CInt
#ccall cef_string_list_value , <cef_string_list_t> -> CInt -> Ptr <cef_string_utf16_t> -> IO CInt
#ccall cef_string_list_append , <cef_string_list_t> -> Ptr <cef_string_utf16_t> -> IO ()
#ccall cef_string_list_clear , <cef_string_list_t> -> IO ()
#ccall cef_string_list_free , <cef_string_list_t> -> IO ()
#ccall cef_string_list_copy , <cef_string_list_t> -> IO <cef_string_list_t>
