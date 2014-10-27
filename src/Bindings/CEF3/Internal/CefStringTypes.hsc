{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "include/internal/cef_string_types.h"
#include "include/internal/cef_build.h"
#include "include/internal/cef_export.h"
module Bindings.CEF3.Internal.CefStringTypes where
import Foreign.Ptr
#strict_import

#if defined(OS_WIN)
#synonym_t char16 , <wchar_t>
#else
{- typedef unsigned short char16; -}
#synonym_t char16 , CUShort
#ifndef WCHAR_T_IS_UTF32
#define WCHAR_T_IS_UTF32
#endif // WCHAR_T_IS_UTF32
#endif // !OS_WIN

{- typedef cef_string_wide_t {
            wchar_t * str; size_t length; void (* dtor)(wchar_t * str);
        } cef_string_wide_t; -}
#starttype cef_string_wide_t
#field str , Ptr CInt
#field length , CSize
#field dtor , FunPtr (Ptr CInt -> IO ())
#stoptype

{- typedef cef_string_utf8_t {
            char * str; size_t length; void (* dtor)(char * str);
        } cef_string_utf8_t; -}
#starttype cef_string_utf8_t
#field str , CString
#field length , CSize
#field dtor , FunPtr (CString -> IO ())
#stoptype

{- typedef cef_string_utf16_t {
            char16 * str; size_t length; void (* dtor)(char16 * str);
        } cef_string_utf16_t; -}
#starttype cef_string_utf16_t
#field str , Ptr CUShort
#field length , CSize
#field dtor , FunPtr (Ptr CUShort -> IO ())
#stoptype

#ccall cef_string_wide_set , Ptr CInt -> CSize -> Ptr <cef_string_wide_t> -> CInt -> IO CInt
#ccall cef_string_utf8_set , CString -> CSize -> Ptr <cef_string_utf8_t> -> CInt -> IO CInt
#ccall cef_string_utf16_set , Ptr CUShort -> CSize -> Ptr <cef_string_utf16_t> -> CInt -> IO CInt


c'cef_string_wide_copy  src src_len output = c'cef_string_wide_set src src_len output 1
c'cef_string_utf8_copy  src src_len output = c'cef_string_utf8_set src src_len output 1
c'cef_string_utf16_copy src src_len output = c'cef_string_utf16_set src src_len output 1



#ccall cef_string_wide_clear , Ptr <cef_string_wide_t> -> IO ()
#ccall cef_string_utf8_clear , Ptr <cef_string_utf8_t> -> IO ()
#ccall cef_string_utf16_clear , Ptr <cef_string_utf16_t> -> IO ()
#ccall cef_string_wide_cmp , Ptr <cef_string_wide_t> -> Ptr <cef_string_wide_t> -> IO CInt
#ccall cef_string_utf8_cmp , Ptr <cef_string_utf8_t> -> Ptr <cef_string_utf8_t> -> IO CInt
#ccall cef_string_utf16_cmp , Ptr <cef_string_utf16_t> -> Ptr <cef_string_utf16_t> -> IO CInt
#ccall cef_string_wide_to_utf8 , Ptr CInt -> CSize -> Ptr <cef_string_utf8_t> -> IO CInt
#ccall cef_string_utf8_to_wide , CString -> CSize -> Ptr <cef_string_wide_t> -> IO CInt
#ccall cef_string_wide_to_utf16 , Ptr CInt -> CSize -> Ptr <cef_string_utf16_t> -> IO CInt
#ccall cef_string_utf16_to_wide , Ptr CUShort -> CSize -> Ptr <cef_string_wide_t> -> IO CInt
#ccall cef_string_utf8_to_utf16 , CString -> CSize -> Ptr <cef_string_utf16_t> -> IO CInt
#ccall cef_string_utf16_to_utf8 , Ptr CUShort -> CSize -> Ptr <cef_string_utf8_t> -> IO CInt
#ccall cef_string_ascii_to_wide , CString -> CSize -> Ptr <cef_string_wide_t> -> IO CInt
#ccall cef_string_ascii_to_utf16 , CString -> CSize -> Ptr <cef_string_utf16_t> -> IO CInt
{- typedef cef_string_wide_t * cef_string_userfree_wide_t; -}
#synonym_t cef_string_userfree_wide_t , Ptr <cef_string_wide_t>
{- typedef cef_string_utf8_t * cef_string_userfree_utf8_t; -}
#synonym_t cef_string_userfree_utf8_t , Ptr <cef_string_utf8_t>
{- typedef cef_string_utf16_t * cef_string_userfree_utf16_t; -}
#synonym_t cef_string_userfree_utf16_t , Ptr <cef_string_utf16_t>
#ccall cef_string_userfree_wide_alloc , IO (<cef_string_userfree_wide_t>)
#ccall cef_string_userfree_utf8_alloc , IO (<cef_string_userfree_utf8_t>)
#ccall cef_string_userfree_utf16_alloc , IO (<cef_string_userfree_utf16_t>)
#ccall cef_string_userfree_wide_free , <cef_string_userfree_wide_t> -> IO ()
#ccall cef_string_userfree_utf8_free , <cef_string_userfree_utf8_t> -> IO ()
#ccall cef_string_userfree_utf16_free , <cef_string_userfree_utf16_t> -> IO ()
