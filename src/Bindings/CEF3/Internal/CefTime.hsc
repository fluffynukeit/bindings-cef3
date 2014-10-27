{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "include/internal/cef_time.h"
#include "include/internal/cef_export.h"
module Bindings.CEF3.Internal.CefTime where
import Foreign.Ptr
#strict_import

{- typedef cef_time_t {
            int year;
            int month;
            int day_of_week;
            int day_of_month;
            int hour;
            int minute;
            int second;
            int millisecond;
        } cef_time_t; -}
#starttype cef_time_t
#field year , CInt
#field month , CInt
#field day_of_week , CInt
#field day_of_month , CInt
#field hour , CInt
#field minute , CInt
#field second , CInt
#field millisecond , CInt
#stoptype
#ccall cef_time_to_timet , Ptr <cef_time_t> -> Ptr CLong -> IO CInt
#ccall cef_time_from_timet , CLong -> Ptr <cef_time_t> -> IO CInt
#ccall cef_time_to_doublet , Ptr <cef_time_t> -> Ptr CDouble -> IO CInt
#ccall cef_time_from_doublet , CDouble -> Ptr <cef_time_t> -> IO CInt
#ccall cef_time_now , Ptr <cef_time_t> -> IO CInt
#ccall cef_time_delta , Ptr <cef_time_t> -> Ptr <cef_time_t> -> Ptr CLong -> IO CInt
