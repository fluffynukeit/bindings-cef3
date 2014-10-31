
#include <bindings.dsl.h>
#include <gtk/gtk.h>

module Gtk where
#strict_import

import Bindings.CEF3

#integral_t GtkWindowType
#num GTK_WINDOW_TOPLEVEL
#num GTK_WINDOW_POPUP


#ccall gtk_init,                    Ptr CInt -> Ptr (Ptr CString) -> IO ()
#ccall gtk_window_new,              <GtkWindowType> -> IO (Ptr <GtkWidget>)
#ccall gtk_window_set_title,        Ptr <GtkWidget> -> CString -> IO ()
-- gtk_window_set_title actually takes Ptr to GtkWindow, but we cheat here
#ccall gtk_window_set_default_size, Ptr <GtkWidget> -> CInt -> CInt -> IO ()

#ccall gtk_vbox_new,                CInt -> CInt -> IO (Ptr <GtkWidget>)
#ccall gtk_container_add,           Ptr a -> Ptr <GtkWidget> -> IO ()
#ccall gtk_widget_show_all,         Ptr <GtkWidget> -> IO ()
#ccall gtk_label_new,               CString -> IO (Ptr <GtkWidget>)
#ccall gtk_frame_new,               CString -> IO (Ptr <GtkWidget>)
#ccall gtk_main,                    IO ()
