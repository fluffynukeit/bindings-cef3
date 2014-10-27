{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "include/internal/cef_types_linux.h"
#include "include/internal/cef_build.h"
#include <gtk/gtk.h>
module Bindings.CEF3.Internal.CefTypesLinux where
import Foreign.Ptr
#strict_import

import Bindings.CEF3.Internal.CefString
{- typedef struct _cef_main_args_t {
            int argc; char * * argv;
        } cef_main_args_t; -}
#starttype cef_main_args_t
#field argc , CInt
#field argv , Ptr CString
#stoptype

#opaque_t GtkWidget
#opaque_t GdkEvent
#opaque_t GdkCursor

#synonym_t cef_cursor_handle_t, Ptr <GdkCursor>
#synonym_t cef_event_handle_t,  Ptr <GdkEvent>
#synonym_t cef_window_handle_t, Ptr <GtkWidget>
#synonym_t cef_text_input_context_t, Ptr ()

{- typedef struct _cef_window_info_t {
            GtkWidget * parent_widget;
            int window_rendering_disabled;
            int transparent_painting;
            GtkWidget * widget;
        } cef_window_info_t; -}
#starttype cef_window_info_t
#field parent_widget , Ptr <GtkWidget>
#field window_rendering_disabled , CInt
#field transparent_painting , CInt
#field widget , Ptr <GtkWidget>
#stoptype
