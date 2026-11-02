-- This file exports all types and functions for the CEF3 framework.  In the 
-- CEF3 C API, classes are exported as structures of function pointers.
-- The classes have lots of interconnected types.  To avoid cyclic imports,
-- they are all defined in the bindings here.
--
-- Because of the small number of individual functions, the functions here
-- are included as well.  This limits the number of binding files
-- significantly.

#include <bindings.dsl.h>
#include <cef_base_capi.h>
#include <internal/cef_export.h>

#include <cef_app_capi.h>
#include <cef_auth_callback_capi.h>
#include <cef_browser_capi.h>
#include <cef_browser_process_handler_capi.h>
#include <cef_callback_capi.h>
#include <cef_client_capi.h>
#include <cef_command_line_capi.h>
#include <cef_context_menu_handler_capi.h>
#include <cef_cookie_capi.h>
#include <cef_dialog_handler_capi.h>
#include <cef_display_handler_capi.h>
#include <cef_dom_capi.h>
#include <cef_download_handler_capi.h>
#include <cef_download_item_capi.h>
#include <cef_drag_data_capi.h>
#include <cef_drag_handler_capi.h>
#include <cef_focus_handler_capi.h>
#include <cef_frame_capi.h>
#include <cef_geolocation_capi.h>
#include <cef_geolocation_handler_capi.h>
#include <cef_jsdialog_handler_capi.h>
#include <cef_keyboard_handler_capi.h>
#include <cef_life_span_handler_capi.h>
#include <cef_load_handler_capi.h>
#include <cef_menu_model_capi.h>
#include <cef_origin_whitelist_capi.h>
#include <cef_path_util_capi.h>
#include <cef_process_message_capi.h>
#include <cef_process_util_capi.h>
#include <cef_render_handler_capi.h>
#include <cef_render_process_handler_capi.h>
#include <cef_request_capi.h>
#include <cef_request_context_capi.h>
#include <cef_request_context_handler_capi.h>
#include <cef_request_handler_capi.h>
#include <cef_resource_bundle_handler_capi.h>
#include <cef_resource_handler_capi.h>
#include <cef_response_capi.h>
#include <cef_scheme_capi.h>
#include <cef_stream_capi.h>
#include <cef_string_visitor_capi.h>
#include <cef_task_capi.h>
#include <cef_trace_capi.h>
#include <cef_url_capi.h>
#include <cef_urlrequest_capi.h>
#include <cef_v8_capi.h>
#include <cef_values_capi.h>
#include <cef_web_plugin_capi.h>
#include <cef_xml_reader_capi.h>
#include <cef_zip_reader_capi.h>

module Bindings.CEF3 
( module Bindings.CEF3
, module Bindings.CEF3.Internal.CefString
, module Bindings.CEF3.Internal.CefStringList
, module Bindings.CEF3.Internal.CefStringMap
, module Bindings.CEF3.Internal.CefStringMultimap
, module Bindings.CEF3.Internal.CefTypes
, module Bindings.CEF3.Internal.CefTime
) where
#strict_import

import Bindings.CEF3.Internal.CefString
import Bindings.CEF3.Internal.CefStringList
import Bindings.CEF3.Internal.CefStringMap
import Bindings.CEF3.Internal.CefStringMultimap
import Bindings.CEF3.Internal.CefTypes
import Bindings.CEF3.Internal.CefTime

-- Haskell type defines
#synonym_t int,     CInt
#synonym_t size_t,  CSize
#synonym_t void,    ()
#synonym_t string,  CString
#synonym_t double,  CDouble
#synonym_t time_t,  CTime

-- Undefine Linxu types.  For some reason they are being substituted here
-- where they shouldn't be.
#undef cef_cursor_handle_t
#undef cef_event_handle_t
#undef cef_window_handle_t
#undef cef_text_input_context_t



#starttype cef_base_t
#field size,      <size_t>
#field add_ref,   <cb_cef_base_add_ref>
#field release,   <cb_cef_base_release>
#field get_refct, <cb_cef_base_get_refct>
#stoptype

#callback_t cb_cef_base_add_ref,   Ptr <cef_base_t> -> IO <int>
#callback_t cb_cef_base_release,   Ptr <cef_base_t> -> IO <int>
#callback_t cb_cef_base_get_refct, Ptr <cef_base_t> -> IO <int>




#starttype cef_app_t
#field base,                              <cef_base_t>
#field on_before_command_line_processing, <cb_cef_app_on_before_command_line_processing>
#field on_register_custom_schemes,        <cb_cef_app_on_register_custom_schemes>
#field get_resource_bundle_handler,       <cb_cef_app_get_resource_bundle_handler>
#field get_browser_process_handler,       <cb_cef_app_get_browser_process_handler>
#field get_render_process_handler,        <cb_cef_app_get_render_process_handler>
#stoptype

#callback_t cb_cef_app_on_before_command_line_processing, Ptr <cef_app_t> -> Ptr <cef_string_t> -> Ptr <cef_command_line_t> -> IO <void>
#callback_t cb_cef_app_on_register_custom_schemes,        Ptr <cef_app_t> -> Ptr <cef_scheme_registrar_t> -> IO <void>
#callback_t cb_cef_app_get_resource_bundle_handler,       Ptr <cef_app_t> -> IO (Ptr <cef_resource_bundle_handler_t>)
#callback_t cb_cef_app_get_browser_process_handler,       Ptr <cef_app_t> -> IO (Ptr <cef_browser_process_handler_t>)
#callback_t cb_cef_app_get_render_process_handler,        Ptr <cef_app_t> -> IO (Ptr <cef_render_process_handler_t>)


#ccall  cef_execute_process,        Ptr <cef_main_args_t> -> Ptr <cef_app_t> -> Ptr <void> -> IO <int>
#ccall  cef_initialize,             Ptr <cef_main_args_t> -> Ptr <cef_settings_t> -> Ptr <cef_app_t> -> Ptr <void> -> IO <int>
#ccall  cef_shutdown,               IO <void>
#ccall  cef_do_message_loop_work,   IO <void>
#ccall  cef_run_message_loop,       IO <void>
#ccall  cef_quit_message_loop,      IO <void>
#ccall  cef_set_osmodal_loop,       <int> -> IO <void>




#starttype cef_auth_callback_t
#field base,    <cef_base_t>
#field cont,    <cb_cef_auth_callback_cont>
#field cancel,  <cb_cef_auth_callback_cancel>
#stoptype

#callback_t cb_cef_auth_callback_cont,    Ptr <cef_auth_callback_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> IO <void>
#callback_t cb_cef_auth_callback_cancel,  Ptr <cef_auth_callback_t> -> IO <void>






#starttype cef_browser_t
#field base,                <cef_base_t>
#field get_host,            <cb_cef_browser_get_host>
#field can_go_back,         <cb_cef_browser_can_go_back>
#field go_back,             <cb_cef_browser_go_back>
#field can_go_forward,      <cb_cef_browser_can_go_forward>
#field go_forward,          <cb_cef_browser_go_forward>
#field is_loading,          <cb_cef_browser_is_loading>
#field reload,              <cb_cef_browser_reload>
#field reload_ignore_cache, <cb_cef_browser_reload_ignore_cache>
#field stop_load,           <cb_cef_browser_stop_load>
#field get_identifier,      <cb_cef_browser_get_identifier>
#field is_same,             <cb_cef_browser_is_same>
#field is_popup,            <cb_cef_browser_is_popup>
#field has_document,        <cb_cef_browser_has_document>
#field get_main_frame,      <cb_cef_browser_get_main_frame>
#field get_focused_frame,   <cb_cef_browser_get_focused_frame>
#field get_frame_byident,   <cb_cef_browser_get_frame_byident>
#field get_frame,           <cb_cef_browser_get_frame>
#field get_frame_count,     <cb_cef_browser_get_frame_count>
#field get_frame_identifiers, <cb_cef_browser_get_frame_identifiers>
#field get_frame_names,     <cb_cef_browser_get_frame_names>
#field send_process_message, <cb_cef_browser_send_process_message>
#stoptype

#callback_t cb_cef_browser_get_host,            Ptr <cef_browser_t> -> IO (Ptr <cef_browser_host_t>)
#callback_t cb_cef_browser_can_go_back,         Ptr <cef_browser_t> -> IO <int>
#callback_t cb_cef_browser_go_back,             Ptr <cef_browser_t> -> IO <int>
#callback_t cb_cef_browser_can_go_forward,      Ptr <cef_browser_t> -> IO <int>
#callback_t cb_cef_browser_go_forward,          Ptr <cef_browser_t> -> IO <int>
#callback_t cb_cef_browser_is_loading,          Ptr <cef_browser_t> -> IO <int>
#callback_t cb_cef_browser_reload,              Ptr <cef_browser_t> -> IO <void>
#callback_t cb_cef_browser_reload_ignore_cache, Ptr <cef_browser_t> -> IO <void>
#callback_t cb_cef_browser_stop_load,           Ptr <cef_browser_t> -> IO <void>
#callback_t cb_cef_browser_get_identifier,      Ptr <cef_browser_t> -> IO <int>
#callback_t cb_cef_browser_is_same,             Ptr <cef_browser_t> -> Ptr <cef_browser_t> -> IO <int>
#callback_t cb_cef_browser_is_popup,            Ptr <cef_browser_t> -> IO <int>
#callback_t cb_cef_browser_has_document,        Ptr <cef_browser_t> -> IO <int>
#callback_t cb_cef_browser_get_main_frame,      Ptr <cef_browser_t> -> IO (Ptr <cef_frame_t>)
#callback_t cb_cef_browser_get_focused_frame,   Ptr <cef_browser_t> -> IO (Ptr <cef_frame_t>)
#callback_t cb_cef_browser_get_frame_byident,   Ptr <cef_browser_t> -> <int64> -> IO (Ptr <cef_frame_t>)
#callback_t cb_cef_browser_get_frame,           Ptr <cef_browser_t> -> Ptr <cef_string_t> -> IO (Ptr <cef_frame_t>)
#callback_t cb_cef_browser_get_frame_count,     Ptr <cef_browser_t> -> IO <size_t>
#callback_t cb_cef_browser_get_frame_identifiers, Ptr <cef_browser_t> -> Ptr <size_t> -> Ptr <int64> -> IO <void>
#callback_t cb_cef_browser_get_frame_names,     Ptr <cef_browser_t> -> <cef_string_list_t> -> IO <void>
#callback_t cb_cef_browser_send_process_message, Ptr <cef_browser_t> -> <cef_process_id_t> -> Ptr <cef_process_message_t> -> IO <int>


#starttype cef_run_file_dialog_callback_t
#field base, <cef_base_t>
#field cont, <cb_cef_run_file_dialog_callback_cont>
#stoptype

#callback_t cb_cef_run_file_dialog_callback_cont, Ptr <cef_browser_t> -> Ptr <cef_browser_host_t> -> <cef_string_list_t> -> IO <void>


#starttype cef_browser_host_t
#field base,                  <cef_base_t>
#field get_browser,           <cb_cef_browser_host_get_browser>
#field parent_window_will_close, <cb_cef_browser_host_parent_window_will_close>
#field close_browser,         <cb_cef_browser_host_close_browser>
#field set_focus,             <cb_cef_browser_host_set_focus>
#field set_window_visibility, <cb_cef_browser_host_set_window_visibility>
#field get_window_handle,     <cb_cef_browser_host_get_window_handle>
#field get_opener_window_handle, <cb_cef_browser_host_get_opener_window_handle>
#field get_client,            <cb_cef_browser_host_get_client>
#field get_request_context,   <cb_cef_browser_host_get_request_context>
#field get_zoom_level,        <cb_cef_browser_host_get_zoom_level>
#field set_zoom_level,        <cb_cef_browser_host_set_zoom_level>
#field run_file_dialog,       <cb_cef_browser_host_run_file_dialog>
#field start_download,        <cb_cef_browser_host_start_download>
#field print,                 <cb_cef_browser_host_print>
#field find,                  <cb_cef_browser_host_find>
#field stop_finding,          <cb_cef_browser_host_stop_finding>
#field show_dev_tools,        <cb_cef_browser_host_show_dev_tools>
#field close_dev_tools,       <cb_cef_browser_host_close_dev_tools>
#field set_mouse_cursor_change_disabled, <cb_cef_browser_host_set_mouse_cursor_change_disabled>
#field is_mouse_cursor_change_disabled, <cb_cef_browser_host_is_mouse_cursor_change_disabled>
#field is_window_rendering_disabled, <cb_cef_browser_host_is_window_rendering_disabled>
#field was_resized,           <cb_cef_browser_host_was_resized>
#field was_hidden,            <cb_cef_browser_host_was_hidden>
#field notify_screen_info_changed, <cb_cef_browser_host_notify_screen_info_changed>
#field invalidate,            <cb_cef_browser_host_invalidate>
#field send_key_event,        <cb_cef_browser_host_send_key_event>
#field send_mouse_click_event, <cb_cef_browser_host_send_mouse_click_event>
#field send_mouse_move_event, <cb_cef_browser_host_send_mouse_move_event>
#field send_mouse_wheel_event, <cb_cef_browser_host_send_mouse_wheel_event>
#field send_focus_event,      <cb_cef_browser_host_send_focus_event>
#field send_capture_lost_event, <cb_cef_browser_host_send_capture_lost_event>
#field get_nstext_input_context, <cb_cef_browser_host_get_nstext_input_context>
#field handle_key_event_before_text_input_client, <cb_cef_browser_host_handle_key_event_before_text_input_client>
#field handle_key_event_after_text_input_client, <cb_cef_browser_host_handle_key_event_after_text_input_client>
#stoptype

#callback_t cb_cef_browser_host_get_browser,           Ptr <cef_browser_host_t> -> IO (Ptr <cef_browser_t>)
#callback_t cb_cef_browser_host_parent_window_will_close, Ptr <cef_browser_host_t> -> IO <void>
#callback_t cb_cef_browser_host_close_browser,         Ptr <cef_browser_host_t> -> <int> -> IO <void>
#callback_t cb_cef_browser_host_set_focus,             Ptr <cef_browser_host_t> -> <int> -> IO <void>
#callback_t cb_cef_browser_host_set_window_visibility, Ptr <cef_browser_host_t> -> <int> -> IO <void>
#callback_t cb_cef_browser_host_get_window_handle,     Ptr <cef_browser_host_t> -> IO <cef_window_handle_t>
#callback_t cb_cef_browser_host_get_opener_window_handle, Ptr <cef_browser_host_t> -> IO <cef_window_handle_t>
#callback_t cb_cef_browser_host_get_client,            Ptr <cef_browser_host_t> -> IO (Ptr <cef_client_t>)
#callback_t cb_cef_browser_host_get_request_context,   Ptr <cef_browser_host_t> -> IO (Ptr <cef_request_context_t>)
#callback_t cb_cef_browser_host_get_zoom_level,        Ptr <cef_browser_host_t> -> IO <double>
#callback_t cb_cef_browser_host_set_zoom_level,        Ptr <cef_browser_host_t> -> <double> -> IO <void>
#callback_t cb_cef_browser_host_run_file_dialog,       Ptr <cef_browser_host_t> -> <cef_file_dialog_mode_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> <cef_string_list_t> -> Ptr <cef_run_file_dialog_callback_t> -> IO <void>
#callback_t cb_cef_browser_host_start_download,        Ptr <cef_browser_host_t> -> Ptr <cef_string_t> -> IO <void>
#callback_t cb_cef_browser_host_print,                 Ptr <cef_browser_host_t> -> IO <void>
#callback_t cb_cef_browser_host_find,                  Ptr <cef_browser_host_t> -> <int> -> Ptr <cef_string_t> -> <int> -> <int> -> <int> -> IO <void>
#callback_t cb_cef_browser_host_stop_finding,          Ptr <cef_browser_host_t> -> <int> -> IO <void>
#callback_t cb_cef_browser_host_show_dev_tools,        Ptr <cef_browser_host_t> -> Ptr <cef_window_info_t> -> Ptr <cef_client_t> -> Ptr <cef_browser_settings_t> -> IO <void>
#callback_t cb_cef_browser_host_close_dev_tools,       Ptr <cef_browser_host_t> -> IO <void>
#callback_t cb_cef_browser_host_set_mouse_cursor_change_disabled, Ptr <cef_browser_host_t> -> <int> -> IO <void>
#callback_t cb_cef_browser_host_is_mouse_cursor_change_disabled, Ptr <cef_browser_host_t> -> IO <int>
#callback_t cb_cef_browser_host_is_window_rendering_disabled, Ptr <cef_browser_host_t> -> IO <int>
#callback_t cb_cef_browser_host_was_resized,           Ptr <cef_browser_host_t> -> IO <void>
#callback_t cb_cef_browser_host_was_hidden,            Ptr <cef_browser_host_t> -> <int> -> IO <void>
#callback_t cb_cef_browser_host_notify_screen_info_changed, Ptr <cef_browser_host_t> -> IO <void>
#callback_t cb_cef_browser_host_invalidate,            Ptr <cef_browser_host_t> -> Ptr <cef_rect_t> -> <cef_paint_element_type_t> -> IO <void>
#callback_t cb_cef_browser_host_send_key_event,        Ptr <cef_browser_host_t> -> Ptr <cef_key_event_t> -> IO <void>
#callback_t cb_cef_browser_host_send_mouse_click_event, Ptr <cef_browser_host_t> -> Ptr <cef_mouse_event_t> -> <cef_mouse_button_type_t> -> <int> -> <int> -> IO <void>
#callback_t cb_cef_browser_host_send_mouse_move_event, Ptr <cef_browser_host_t> -> Ptr <cef_mouse_event_t> -> <int> -> IO <void>
#callback_t cb_cef_browser_host_send_mouse_wheel_event, Ptr <cef_browser_host_t> -> Ptr <cef_mouse_event_t> -> <int> -> <int> -> IO <void>
#callback_t cb_cef_browser_host_send_focus_event,      Ptr <cef_browser_host_t> -> <int> -> IO <void>
#callback_t cb_cef_browser_host_send_capture_lost_event, Ptr <cef_browser_host_t> -> IO <void>
#callback_t cb_cef_browser_host_get_nstext_input_context, Ptr <cef_browser_host_t> -> IO <cef_text_input_context_t>
#callback_t cb_cef_browser_host_handle_key_event_before_text_input_client, Ptr <cef_browser_host_t> -> <cef_event_handle_t> -> IO <void>
#callback_t cb_cef_browser_host_handle_key_event_after_text_input_client, Ptr <cef_browser_host_t> -> <cef_event_handle_t> -> IO <void>


#ccall cef_browser_host_create_browser, Ptr <cef_window_info_t> -> Ptr <cef_client_t> -> Ptr <cef_string_t> -> Ptr <cef_browser_settings_t> -> Ptr <cef_request_context_t> -> IO <int>
#ccall cef_browser_host_create_browser_sync, Ptr <cef_window_info_t> -> Ptr <cef_client_t> -> Ptr <cef_string_t> -> Ptr <cef_browser_settings_t> -> Ptr <cef_request_context_t> -> IO (Ptr <cef_browser_t>)




#starttype cef_browser_process_handler_t
#field base,                              <cef_base_t>
#field on_context_initialized,            <cb_cef_browser_process_handler_on_context_initialized>
#field on_before_child_process_launch,    <cb_cef_browser_process_handler_on_before_child_process_launch>
#field on_render_process_thread_created,  <cb_cef_browser_process_handler_on_render_process_thread_created>
#stoptype

#callback_t cb_cef_browser_process_handler_on_context_initialized,            Ptr <cef_browser_process_handler_t> -> IO <void>
#callback_t cb_cef_browser_process_handler_on_before_child_process_launch,    Ptr <cef_browser_process_handler_t> -> Ptr <cef_command_line_t> -> IO <void>
#callback_t cb_cef_browser_process_handler_on_render_process_thread_created,  Ptr <cef_browser_process_handler_t> -> Ptr <cef_list_value_t> -> IO <void>






#starttype cef_callback_t
#field base,    <cef_base_t>
#field cont,    <cb_cef_callback_cont>
#field cancel,  <cb_cef_callback_cancel>
#stoptype

#callback_t cb_cef_callback_cont,    Ptr <cef_callback_t> -> IO <void>
#callback_t cb_cef_callback_cancel,  Ptr <cef_callback_t> -> IO <void>


#starttype cef_completion_handler_t
#field base,        <cef_base_t>
#field on_complete, <cb_cef_completion_handler_on_complete>
#stoptype

#callback_t cb_cef_completion_handler_on_complete, Ptr <cef_completion_handler_t> -> IO <void>






#starttype cef_client_t
#field base,                      <cef_base_t>
#field get_context_menu_handler,  <cb_cef_client_get_context_menu_handler>
#field get_dialog_handler,        <cb_cef_client_get_dialog_handler>
#field get_display_handler,       <cb_cef_client_get_display_handler>
#field get_download_handler,      <cb_cef_client_get_download_handler>
#field get_drag_handler,          <cb_cef_client_get_drag_handler>
#field get_focus_handler,         <cb_cef_client_get_focus_handler>
#field get_geolocation_handler,   <cb_cef_client_get_geolocation_handler>
#field get_jsdialog_handler,      <cb_cef_client_get_jsdialog_handler>
#field get_keyboard_handler,      <cb_cef_client_get_keyboard_handler>
#field get_life_span_handler,     <cb_cef_client_get_life_span_handler>
#field get_load_handler,          <cb_cef_client_get_load_handler>
#field get_render_handler,        <cb_cef_client_get_render_handler>
#field get_request_handler,       <cb_cef_client_get_request_handler>
#field on_process_message_received, <cb_cef_client_on_process_message_received>
#stoptype

#callback_t cb_cef_client_get_context_menu_handler,  Ptr <cef_client_t> -> IO (Ptr <cef_context_menu_handler_t>)
#callback_t cb_cef_client_get_dialog_handler,        Ptr <cef_client_t> -> IO (Ptr <cef_dialog_handler_t>)
#callback_t cb_cef_client_get_display_handler,       Ptr <cef_client_t> -> IO (Ptr <cef_display_handler_t>)
#callback_t cb_cef_client_get_download_handler,      Ptr <cef_client_t> -> IO (Ptr <cef_download_handler_t>)
#callback_t cb_cef_client_get_drag_handler,          Ptr <cef_client_t> -> IO (Ptr <cef_drag_handler_t>)
#callback_t cb_cef_client_get_focus_handler,         Ptr <cef_client_t> -> IO (Ptr <cef_focus_handler_t>)
#callback_t cb_cef_client_get_geolocation_handler,   Ptr <cef_client_t> -> IO (Ptr <cef_geolocation_handler_t>)
#callback_t cb_cef_client_get_jsdialog_handler,      Ptr <cef_client_t> -> IO (Ptr <cef_jsdialog_handler_t>)
#callback_t cb_cef_client_get_keyboard_handler,      Ptr <cef_client_t> -> IO (Ptr <cef_keyboard_handler_t>)
#callback_t cb_cef_client_get_life_span_handler,     Ptr <cef_client_t> -> IO (Ptr <cef_life_span_handler_t>)
#callback_t cb_cef_client_get_load_handler,          Ptr <cef_client_t> -> IO (Ptr <cef_load_handler_t>)
#callback_t cb_cef_client_get_render_handler,        Ptr <cef_client_t> -> IO (Ptr <cef_render_handler_t>)
#callback_t cb_cef_client_get_request_handler,       Ptr <cef_client_t> -> IO (Ptr <cef_request_handler_t>)
#callback_t cb_cef_client_on_process_message_received, Ptr <cef_client_t> -> Ptr <cef_browser_t> -> <cef_process_id_t> -> Ptr <cef_process_message_t> -> IO <int>






#starttype cef_command_line_t
#field base,              <cef_base_t>
#field is_valid,          <cb_cef_command_line_is_valid>
#field is_read_only,      <cb_cef_command_line_is_read_only>
#field copy,              <cb_cef_command_line_copy>
#field init_from_argv,    <cb_cef_command_line_init_from_argv>
#field init_from_string,  <cb_cef_command_line_init_from_string>
#field reset,             <cb_cef_command_line_reset>
#field get_argv,          <cb_cef_command_line_get_argv>
#field get_command_line_string, <cb_cef_command_line_get_command_line_string>
#field get_program,       <cb_cef_command_line_get_program>
#field set_program,       <cb_cef_command_line_set_program>
#field has_switches,      <cb_cef_command_line_has_switches>
#field has_switch,        <cb_cef_command_line_has_switch>
#field get_switch_value,  <cb_cef_command_line_get_switch_value>
#field get_switches,      <cb_cef_command_line_get_switches>
#field append_switch,     <cb_cef_command_line_append_switch>
#field append_switch_with_value, <cb_cef_command_line_append_switch_with_value>
#field has_arguments,     <cb_cef_command_line_has_arguments>
#field get_arguments,     <cb_cef_command_line_get_arguments>
#field append_argument,   <cb_cef_command_line_append_argument>
#field prepend_wrapper,   <cb_cef_command_line_prepend_wrapper>
#stoptype 

#callback_t cb_cef_command_line_is_valid,          Ptr <cef_command_line_t> -> IO <int>
#callback_t cb_cef_command_line_is_read_only,      Ptr <cef_command_line_t> -> IO <int>
#callback_t cb_cef_command_line_copy,              Ptr <cef_command_line_t> -> IO (Ptr <cef_command_line_t>)
#callback_t cb_cef_command_line_init_from_argv,    Ptr <cef_command_line_t> -> <int> -> Ptr <string> -> IO <void>
#callback_t cb_cef_command_line_init_from_string,  Ptr <cef_command_line_t> -> Ptr <cef_string_t> -> IO <void>
#callback_t cb_cef_command_line_reset,             Ptr <cef_command_line_t> -> IO <void>
#callback_t cb_cef_command_line_get_argv,          Ptr <cef_command_line_t> -> <cef_string_list_t> -> IO <void>
#callback_t cb_cef_command_line_get_command_line_string, Ptr <cef_command_line_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_command_line_get_program,       Ptr <cef_command_line_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_command_line_set_program,       Ptr <cef_command_line_t> -> Ptr <cef_string_t> -> IO <void>
#callback_t cb_cef_command_line_has_switches,      Ptr <cef_command_line_t> -> IO <int>
#callback_t cb_cef_command_line_has_switch,        Ptr <cef_command_line_t> -> Ptr <cef_string_t> -> IO <int>
#callback_t cb_cef_command_line_get_switch_value,  Ptr <cef_command_line_t> -> Ptr <cef_string_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_command_line_get_switches,      Ptr <cef_command_line_t> -> <cef_string_map_t> -> IO <void>
#callback_t cb_cef_command_line_append_switch,     Ptr <cef_command_line_t> -> Ptr <cef_string_t> -> IO <void>
#callback_t cb_cef_command_line_append_switch_with_value, Ptr <cef_command_line_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> IO <void>
#callback_t cb_cef_command_line_has_arguments,     Ptr <cef_command_line_t> -> IO <int>
#callback_t cb_cef_command_line_get_arguments,     Ptr <cef_command_line_t> -> <cef_string_list_t> -> IO <void>
#callback_t cb_cef_command_line_append_argument,   Ptr <cef_command_line_t> -> Ptr <cef_string_t> -> IO <void>
#callback_t cb_cef_command_line_prepend_wrapper,   Ptr <cef_command_line_t> -> Ptr <cef_string_t> -> IO <void>


#ccall cef_command_line_create,     IO (Ptr <cef_command_line_t>)
#ccall cef_command_line_get_global, IO (Ptr <cef_command_line_t>)




#starttype cef_context_menu_handler_t
#field base,                      <cef_base_t>
#field on_before_context_menu,    <cb_cef_context_menu_handler_on_before_context_menu>
#field on_context_menu_command,   <cb_cef_context_menu_handler_on_context_menu_command>
#field on_context_menu_dismissed, <cb_cef_context_menu_handler_on_context_menu_dismissed>
#stoptype

#callback_t cb_cef_context_menu_handler_on_before_context_menu,    Ptr <cef_context_menu_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> Ptr <cef_context_menu_params_t> -> Ptr <cef_menu_model_t> -> IO <void>
#callback_t cb_cef_context_menu_handler_on_context_menu_command,   Ptr <cef_context_menu_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> Ptr <cef_context_menu_params_t> -> <int> -> <cef_event_flags_t> -> IO <int>
#callback_t cb_cef_context_menu_handler_on_context_menu_dismissed, Ptr <cef_context_menu_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> IO <void>


#starttype cef_context_menu_params_t
#field base,                <cef_base_t>
#field get_xcoord,          <cb_cef_context_menu_params_get_xcoord>
#field get_ycoord,          <cb_cef_context_menu_params_get_ycoord>
#field get_type_flags,      <cb_cef_context_menu_params_get_type_flags>
#field get_link_url,        <cb_cef_context_menu_params_get_link_url>
#field get_unfiltered_link_url, <cb_cef_context_menu_params_get_unfiltered_link_url>
#field get_source_url,      <cb_cef_context_menu_params_get_source_url>
#field has_image_contents,  <cb_cef_context_menu_params_has_image_contents>
#field get_page_url,        <cb_cef_context_menu_params_get_page_url>
#field get_frame_url,       <cb_cef_context_menu_params_get_frame_url>
#field get_frame_charset,   <cb_cef_context_menu_params_get_frame_charset>
#field get_media_type,      <cb_cef_context_menu_params_get_media_type>
#field get_media_state_flags, <cb_cef_context_menu_params_get_media_state_flags>
#field get_selection_text,  <cb_cef_context_menu_params_get_selection_text>
#field is_editable,         <cb_cef_context_menu_params_is_editable>
#field is_speech_input_enabled, <cb_cef_context_menu_params_is_speech_input_enabled>
#field get_edit_state_flags, <cb_cef_context_menu_params_get_edit_state_flags>
#stoptype

#callback_t cb_cef_context_menu_params_get_xcoord,          Ptr <cef_context_menu_params_t> -> IO <int>
#callback_t cb_cef_context_menu_params_get_ycoord,          Ptr <cef_context_menu_params_t> -> IO <int>
#callback_t cb_cef_context_menu_params_get_type_flags,      Ptr <cef_context_menu_params_t> -> IO <cef_context_menu_type_flags_t>
#callback_t cb_cef_context_menu_params_get_link_url,        Ptr <cef_context_menu_params_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_context_menu_params_get_unfiltered_link_url, Ptr <cef_context_menu_params_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_context_menu_params_get_source_url,      Ptr <cef_context_menu_params_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_context_menu_params_has_image_contents,  Ptr <cef_context_menu_params_t> -> IO <int>
#callback_t cb_cef_context_menu_params_get_page_url,        Ptr <cef_context_menu_params_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_context_menu_params_get_frame_url,       Ptr <cef_context_menu_params_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_context_menu_params_get_frame_charset,   Ptr <cef_context_menu_params_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_context_menu_params_get_media_type,      Ptr <cef_context_menu_params_t> -> IO <cef_context_menu_media_type_t>
#callback_t cb_cef_context_menu_params_get_media_state_flags, Ptr <cef_context_menu_params_t> -> IO <cef_context_menu_media_state_flags_t>
#callback_t cb_cef_context_menu_params_get_selection_text,  Ptr <cef_context_menu_params_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_context_menu_params_is_editable,         Ptr <cef_context_menu_params_t> -> IO <int>
#callback_t cb_cef_context_menu_params_is_speech_input_enabled, Ptr <cef_context_menu_params_t> -> IO <int>
#callback_t cb_cef_context_menu_params_get_edit_state_flags, Ptr <cef_context_menu_params_t> -> IO <cef_context_menu_edit_state_flags_t>







#starttype cef_cookie_manager_t
#field base,                  <cef_base_t>
#field set_supported_schemes, <cb_cef_cookie_manager_set_supported_schemes>
#field visit_all_cookies,     <cb_cef_cookie_manager_visit_all_cookies>
#field visit_url_cookies,     <cb_cef_cookie_manager_visit_url_cookies>
#field set_cookie,            <cb_cef_cookie_manager_set_cookie>
#field delete_cookies,        <cb_cef_cookie_manager_delete_cookies>
#field set_storage_path,      <cb_cef_cookie_manager_set_storage_path>
#field flush_store,           <cb_cef_cookie_manager_flush_store>
#stoptype

#callback_t cb_cef_cookie_manager_set_supported_schemes, Ptr <cef_cookie_manager_t> -> <cef_string_list_t> -> IO <void>
#callback_t cb_cef_cookie_manager_visit_all_cookies,     Ptr <cef_cookie_manager_t> -> Ptr <cef_cookie_visitor_t> -> IO <int>
#callback_t cb_cef_cookie_manager_visit_url_cookies,     Ptr <cef_cookie_manager_t> -> Ptr <cef_string_t> -> <int> -> Ptr <cef_cookie_visitor_t> -> IO <int>
#callback_t cb_cef_cookie_manager_set_cookie,            Ptr <cef_cookie_manager_t> -> Ptr <cef_string_t> -> Ptr <cef_cookie_t> -> IO <int>
#callback_t cb_cef_cookie_manager_delete_cookies,        Ptr <cef_cookie_manager_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> IO <int>
#callback_t cb_cef_cookie_manager_set_storage_path,      Ptr <cef_cookie_manager_t> -> Ptr <cef_string_t> -> <int> -> IO <int>
#callback_t cb_cef_cookie_manager_flush_store,           Ptr <cef_cookie_manager_t> -> Ptr <cef_completion_handler_t> -> IO <int>


#starttype cef_cookie_visitor_t
#field base,  <cef_base_t>
#field visit, <cb_cef_cookie_visitor_visit>
#stoptype

#callback_t cb_cef_cookie_visitor_visit, Ptr <cef_cookie_visitor_t> -> Ptr <cef_cookie_t> -> <int> -> <int> -> Ptr <int> -> IO <int>


#ccall cef_cookie_manager_get_global_manager,   IO (Ptr <cef_cookie_manager_t>)
#ccall cef_cookie_manager_create_manager,       Ptr <cef_string_t> -> <int> -> IO (Ptr <cef_cookie_manager_t>)





#starttype cef_file_dialog_callback_t
#field base,    <cef_base_t>
#field cont,    <cb_cef_file_dialog_callback_cont>
#field cancel,  <cb_cef_file_dialog_callback_cancel>
#stoptype

#callback_t cb_cef_file_dialog_callback_cont,    Ptr <cef_file_dialog_callback_t> -> <cef_string_list_t> -> IO <void>
#callback_t cb_cef_file_dialog_callback_cancel,  Ptr <cef_file_dialog_callback_t> -> IO <void>


#starttype cef_dialog_handler_t
#field base,            <cef_base_t>
#field on_file_dialog,  <cb_cef_dialog_handler_on_file_dialog>
#stoptype

#callback_t cb_cef_dialog_handler_on_file_dialog,  Ptr <cef_dialog_handler_t> -> Ptr <cef_browser_t> -> <cef_file_dialog_mode_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> <cef_string_list_t> -> Ptr <cef_file_dialog_callback_t> -> IO <int>





#starttype cef_display_handler_t
#field base,                <cef_base_t>
#field on_address_change,   <cb_cef_display_handler_on_address_change>
#field on_title_change,     <cb_cef_display_handler_on_title_change>
#field on_tooltip,          <cb_cef_display_handler_on_tooltip>
#field on_status_message,   <cb_cef_display_handler_on_status_message>
#field on_console_message,  <cb_cef_display_handler_on_console_message>
#stoptype

#callback_t cb_cef_display_handler_on_address_change,   Ptr <cef_display_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> Ptr <cef_string_t> -> IO <void>
#callback_t cb_cef_display_handler_on_title_change,     Ptr <cef_display_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_string_t> -> IO <void>
#callback_t cb_cef_display_handler_on_tooltip,          Ptr <cef_display_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_string_t> -> IO <int>
#callback_t cb_cef_display_handler_on_status_message,   Ptr <cef_display_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_string_t> -> IO <void>
#callback_t cb_cef_display_handler_on_console_message,  Ptr <cef_display_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> <int> -> IO <int>




#starttype cef_domvisitor_t
#field base,    <cef_base_t>
#field visit,   <cb_cef_domvisitor_visit>
#stoptype

#callback_t cb_cef_domvisitor_visit,   Ptr <cef_domvisitor_t> -> Ptr <cef_domdocument_t> -> IO <void>


#starttype cef_domdocument_t
#field base,                <cef_base_t>
#field get_type,            <cb_cef_domdocument_get_type>
#field get_document,        <cb_cef_domdocument_get_document>
#field get_body,            <cb_cef_domdocument_get_body>
#field get_head,            <cb_cef_domdocument_get_head>
#field get_title,           <cb_cef_domdocument_get_title>
#field get_element_by_id,   <cb_cef_domdocument_get_element_by_id>
#field get_focused_node,    <cb_cef_domdocument_get_focused_node>
#field has_selection,       <cb_cef_domdocument_has_selection>
#field get_selection_start_node,    <cb_cef_domdocument_get_selection_start_node>
#field get_selection_start_offset,  <cb_cef_domdocument_get_selection_start_offset>
#field get_selection_end_node,      <cb_cef_domdocument_get_selection_end_node>
#field get_selection_end_offset,    <cb_cef_domdocument_get_selection_end_offset>
#field get_selection_as_markup,     <cb_cef_domdocument_get_selection_as_markup>
#field get_selection_as_text,       <cb_cef_domdocument_get_selection_as_text>
#field get_base_url,        <cb_cef_domdocument_get_base_url>
#field get_complete_url,    <cb_cef_domdocument_get_complete_url>
#stoptype

#callback_t cb_cef_domdocument_get_type,            Ptr <cef_domdocument_t> -> IO <cef_dom_document_type_t>
#callback_t cb_cef_domdocument_get_document,        Ptr <cef_domdocument_t> -> IO (Ptr <cef_domnode_t>)
#callback_t cb_cef_domdocument_get_body,            Ptr <cef_domdocument_t> -> IO (Ptr <cef_domnode_t>)
#callback_t cb_cef_domdocument_get_head,            Ptr <cef_domdocument_t> -> IO (Ptr <cef_domnode_t>)
#callback_t cb_cef_domdocument_get_title,           Ptr <cef_domdocument_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_domdocument_get_element_by_id,   Ptr <cef_domdocument_t> -> Ptr <cef_string_t> -> IO (Ptr <cef_domnode_t>)
#callback_t cb_cef_domdocument_get_focused_node,    Ptr <cef_domdocument_t> -> IO (Ptr <cef_domnode_t>)
#callback_t cb_cef_domdocument_has_selection,       Ptr <cef_domdocument_t> -> IO <int>
#callback_t cb_cef_domdocument_get_selection_start_node,    Ptr <cef_domdocument_t> -> IO (Ptr <cef_domnode_t>)
#callback_t cb_cef_domdocument_get_selection_start_offset,  Ptr <cef_domdocument_t> -> IO <int>
#callback_t cb_cef_domdocument_get_selection_end_node,      Ptr <cef_domdocument_t> -> IO (Ptr <cef_domnode_t>)
#callback_t cb_cef_domdocument_get_selection_end_offset,    Ptr <cef_domdocument_t> -> IO <int>
#callback_t cb_cef_domdocument_get_selection_as_markup,     Ptr <cef_domdocument_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_domdocument_get_selection_as_text,       Ptr <cef_domdocument_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_domdocument_get_base_url,        Ptr <cef_domdocument_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_domdocument_get_complete_url,    Ptr <cef_domdocument_t> -> Ptr <cef_string_t> -> IO <cef_string_userfree_t>


#starttype cef_domnode_t
#field base,        <cef_base_t>
#field get_type,    <cb_cef_domnode_get_type>
#field is_text,     <cb_cef_domnode_is_text>
#field is_element,  <cb_cef_domnode_is_element>
#field is_editable, <cb_cef_domnode_is_editable>
#field is_form_control_element,       <cb_cef_domnode_is_form_control_element>
#field get_form_control_element_type, <cb_cef_domnode_get_form_control_element_type>
#field is_same,     <cb_cef_domnode_is_same>
#field get_name,    <cb_cef_domnode_get_name>
#field get_value,   <cb_cef_domnode_get_value>
#field set_value,   <cb_cef_domnode_set_value>
#field get_as_markup, <cb_cef_domnode_get_as_markup>
#field get_document,  <cb_cef_domnode_get_document>
#field get_parent,    <cb_cef_domnode_get_parent>
#field get_previous_sibling,  <cb_cef_domnode_get_previous_sibling>
#field get_next_sibling,      <cb_cef_domnode_get_next_sibling>
#field has_children,          <cb_cef_domnode_has_children>
#field get_first_child,       <cb_cef_domnode_get_first_child>
#field get_last_child,        <cb_cef_domnode_get_last_child>
#field add_event_listener,    <cb_cef_domnode_add_event_listener>
#field get_element_tag_name,    <cb_cef_domnode_get_element_tag_name>
#field has_element_attributes,  <cb_cef_domnode_has_element_attributes>
#field has_element_attribute,   <cb_cef_domnode_has_element_attribute>
#field get_element_attribute,   <cb_cef_domnode_get_element_attribute>
#field get_element_attributes,  <cb_cef_domnode_get_element_attributes>
#field set_element_attribute,   <cb_cef_domnode_set_element_attribute>
#field get_element_inner_text,  <cb_cef_domnode_get_element_inner_text>
#stoptype

#callback_t cb_cef_domnode_get_type,    Ptr <cef_domnode_t> -> IO <cef_dom_node_type_t>
#callback_t cb_cef_domnode_is_text,     Ptr <cef_domnode_t> -> IO <int>
#callback_t cb_cef_domnode_is_element,  Ptr <cef_domnode_t> -> IO <int>
#callback_t cb_cef_domnode_is_editable, Ptr <cef_domnode_t> -> IO <int>
#callback_t cb_cef_domnode_is_form_control_element,       Ptr <cef_domnode_t> -> IO <int>
#callback_t cb_cef_domnode_get_form_control_element_type, Ptr <cef_domnode_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_domnode_is_same,     Ptr <cef_domnode_t> -> Ptr <cef_domnode_t> -> IO <int>
#callback_t cb_cef_domnode_get_name,    Ptr <cef_domnode_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_domnode_get_value,   Ptr <cef_domnode_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_domnode_set_value,   Ptr <cef_domnode_t> -> Ptr <cef_string_t> -> IO <int>
#callback_t cb_cef_domnode_get_as_markup, Ptr <cef_domnode_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_domnode_get_document,  Ptr <cef_domnode_t> -> IO (Ptr <cef_domdocument_t>)
#callback_t cb_cef_domnode_get_parent,    Ptr <cef_domnode_t> -> IO (Ptr <cef_domnode_t>)
#callback_t cb_cef_domnode_get_previous_sibling,  Ptr <cef_domnode_t> -> IO (Ptr <cef_domnode_t>)
#callback_t cb_cef_domnode_get_next_sibling,      Ptr <cef_domnode_t> -> IO (Ptr <cef_domnode_t>)
#callback_t cb_cef_domnode_has_children,          Ptr <cef_domnode_t> -> IO <int>
#callback_t cb_cef_domnode_get_first_child,       Ptr <cef_domnode_t> -> IO (Ptr <cef_domnode_t>)
#callback_t cb_cef_domnode_get_last_child,        Ptr <cef_domnode_t> -> IO (Ptr <cef_domnode_t>)
#callback_t cb_cef_domnode_add_event_listener,    Ptr <cef_domnode_t> -> Ptr <cef_string_t> -> Ptr <cef_domevent_listener_t> -> <int> -> IO <void>
#callback_t cb_cef_domnode_get_element_tag_name,    Ptr <cef_domnode_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_domnode_has_element_attributes,  Ptr <cef_domnode_t> -> IO <int>
#callback_t cb_cef_domnode_has_element_attribute,   Ptr <cef_domnode_t> -> Ptr <cef_string_t> -> IO <int>
#callback_t cb_cef_domnode_get_element_attribute,   Ptr <cef_domnode_t> -> Ptr <cef_string_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_domnode_get_element_attributes,  Ptr <cef_domnode_t> -> <cef_string_map_t> -> IO <void>
#callback_t cb_cef_domnode_set_element_attribute,   Ptr <cef_domnode_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> IO <int>
#callback_t cb_cef_domnode_get_element_inner_text,  Ptr <cef_domnode_t> -> IO <cef_string_userfree_t>


#starttype cef_domevent_t
#field base,          <cef_base_t>
#field get_type,      <cb_cef_domevent_get_type>
#field get_category,  <cb_cef_domevent_get_category>
#field get_phase,     <cb_cef_domevent_get_phase>
#field can_bubble,    <cb_cef_domevent_can_bubble>
#field can_cancel,    <cb_cef_domevent_can_cancel>
#field get_document,  <cb_cef_domevent_get_document>
#field get_target,    <cb_cef_domevent_get_target>
#field get_current_target, <cb_cef_domevent_get_current_target>
#stoptype

#callback_t cb_cef_domevent_get_type,      Ptr <cef_domevent_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_domevent_get_category,  Ptr <cef_domevent_t> -> IO <cef_dom_event_category_t>
#callback_t cb_cef_domevent_get_phase,     Ptr <cef_domevent_t> -> IO <cef_dom_event_phase_t>
#callback_t cb_cef_domevent_can_bubble,    Ptr <cef_domevent_t> -> IO <int>
#callback_t cb_cef_domevent_can_cancel,    Ptr <cef_domevent_t> -> IO <int>
#callback_t cb_cef_domevent_get_document,  Ptr <cef_domevent_t> -> IO (Ptr <cef_domdocument_t>)
#callback_t cb_cef_domevent_get_target,    Ptr <cef_domevent_t> -> IO (Ptr <cef_domnode_t>)
#callback_t cb_cef_domevent_get_current_target, Ptr <cef_domevent_t> -> IO (Ptr <cef_domnode_t>)


#starttype cef_domevent_listener_t
#field base,          <cef_base_t>
#field handle_event,  <cb_cef_domevent_listener_handle_event>
#stoptype

#callback_t cb_cef_domevent_listener_handle_event,  Ptr <cef_domevent_listener_t> -> Ptr <cef_domevent_t> -> IO <void>






#starttype cef_before_download_callback_t
#field base,  <cef_base_t>
#field cont,  <cb_cef_before_download_callback_cont>
#stoptype

#callback_t cb_cef_before_download_callback_cont,  Ptr <cef_string_t> -> <int> -> IO <void>


#starttype cef_download_item_callback_t
#field base,    <cef_base_t>
#field cancel,  <cb_cef_download_item_callback_cancel>
#stoptype

#callback_t cb_cef_download_item_callback_cancel,  Ptr <cef_download_item_callback_t> -> IO <void>


#starttype cef_download_handler_t
#field base,                <cef_base_t>
#field on_before_download,  <cb_cef_download_handler_on_before_download>
#field on_download_updated, <cb_cef_download_handler_on_download_updated>
#stoptype

#callback_t cb_cef_download_handler_on_before_download,  Ptr <cef_download_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_download_item_t> -> Ptr <cef_string_t> -> Ptr <cef_before_download_callback_t> -> IO <void>
#callback_t cb_cef_download_handler_on_download_updated, Ptr <cef_download_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_download_item_t> -> Ptr <cef_download_item_callback_t> -> IO <void>






#starttype cef_download_item_t
#field base,                    <cef_base_t>
#field is_valid,                <cb_cef_download_item_is_valid>
#field is_in_progress,          <cb_cef_download_item_is_in_progress>
#field is_complete,             <cb_cef_download_item_is_complete>
#field is_canceled,             <cb_cef_download_item_is_canceled>
#field get_current_speed,       <cb_cef_download_item_get_current_speed>
#field get_percent_complete,    <cb_cef_download_item_get_percent_complete>
#field get_total_bytes,         <cb_cef_download_item_get_total_bytes>
#field get_received_bytes,      <cb_cef_download_item_get_received_bytes>
#field get_start_time,          FunPtr (Ptr <cef_download_item_t> -> IO <cef_time_t>)
#field get_end_time,            FunPtr (Ptr <cef_download_item_t> -> IO <cef_time_t>)
#field get_full_path,           <cb_cef_download_item_get_full_path>
#field get_id,                  <cb_cef_download_item_get_id>
#field get_url,                 <cb_cef_download_item_get_url>
#field get_suggested_file_name, <cb_cef_download_item_get_suggested_file_name>
#field get_content_disposition, <cb_cef_download_item_get_content_disposition>
#field get_mime_type,           <cb_cef_download_item_get_mime_type>
#stoptype

#callback_t cb_cef_download_item_is_valid,                Ptr <cef_download_item_t> -> IO <int>
#callback_t cb_cef_download_item_is_in_progress,          Ptr <cef_download_item_t> -> IO <int>
#callback_t cb_cef_download_item_is_complete,             Ptr <cef_download_item_t> -> IO <int>
#callback_t cb_cef_download_item_is_canceled,             Ptr <cef_download_item_t> -> IO <int>
#callback_t cb_cef_download_item_get_current_speed,       Ptr <cef_download_item_t> -> IO <int64>
#callback_t cb_cef_download_item_get_percent_complete,    Ptr <cef_download_item_t> -> IO <int>
#callback_t cb_cef_download_item_get_total_bytes,         Ptr <cef_download_item_t> -> IO <int64>
#callback_t cb_cef_download_item_get_received_bytes,      Ptr <cef_download_item_t> -> IO <int64>
#callback_t cb_cef_download_item_get_full_path,           Ptr <cef_download_item_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_download_item_get_id,                  Ptr <cef_download_item_t> -> IO <uint32>
#callback_t cb_cef_download_item_get_url,                 Ptr <cef_download_item_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_download_item_get_suggested_file_name, Ptr <cef_download_item_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_download_item_get_content_disposition, Ptr <cef_download_item_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_download_item_get_mime_type,           Ptr <cef_download_item_t> -> IO <cef_string_userfree_t>






#starttype cef_drag_data_t
#field base,                <cef_base_t>
#field is_link,             <cb_cef_drag_data_is_link>
#field is_fragment,         <cb_cef_drag_data_is_fragment>
#field is_file,             <cb_cef_drag_data_is_file>
#field get_link_url,        <cb_cef_drag_data_get_link_url>
#field get_link_title,      <cb_cef_drag_data_get_link_title>
#field get_link_metadata,   <cb_cef_drag_data_get_link_metadata>
#field get_fragment_text,   <cb_cef_drag_data_get_fragment_text>
#field get_fragment_html,   <cb_cef_drag_data_get_fragment_html>
#field get_fragment_base_url, <cb_cef_drag_data_get_fragment_base_url>
#field get_file_name,       <cb_cef_drag_data_get_file_name>
#field get_file_names,      <cb_cef_drag_data_get_file_names>
#stoptype

#callback_t cb_cef_drag_data_is_link,             Ptr <cef_drag_data_t> -> IO <int>
#callback_t cb_cef_drag_data_is_fragment,         Ptr <cef_drag_data_t> -> IO <int>
#callback_t cb_cef_drag_data_is_file,             Ptr <cef_drag_data_t> -> IO <int>
#callback_t cb_cef_drag_data_get_link_url,        Ptr <cef_drag_data_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_drag_data_get_link_title,      Ptr <cef_drag_data_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_drag_data_get_link_metadata,   Ptr <cef_drag_data_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_drag_data_get_fragment_text,   Ptr <cef_drag_data_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_drag_data_get_fragment_html,   Ptr <cef_drag_data_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_drag_data_get_fragment_base_url, Ptr <cef_drag_data_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_drag_data_get_file_name,       Ptr <cef_drag_data_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_drag_data_get_file_names,      Ptr <cef_drag_data_t> -> <cef_string_list_t> -> IO <int>






#starttype cef_drag_handler_t
#field base,          <cef_base_t>
#field on_drag_enter, <cb_cef_drag_handler_on_drag_enter>
#stoptype

#callback_t cb_cef_drag_handler_on_drag_enter, Ptr <cef_drag_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_drag_data_t> -> <cef_drag_operations_mask_t> -> IO <int>





#starttype cef_focus_handler_t
#field base,            <cef_base_t>
#field on_take_focus,   <cb_cef_focus_handler_on_take_focus>
#field on_set_focus,    <cb_cef_focus_handler_on_set_focus>
#field on_got_focus,    <cb_cef_focus_handler_on_got_focus>
#stoptype

#callback_t cb_cef_focus_handler_on_take_focus,   Ptr <cef_focus_handler_t> -> Ptr <cef_browser_t> -> <int> -> IO <void>
#callback_t cb_cef_focus_handler_on_set_focus,    Ptr <cef_focus_handler_t> -> Ptr <cef_browser_t> -> <cef_focus_source_t> -> IO <int>
#callback_t cb_cef_focus_handler_on_got_focus,    Ptr <cef_focus_handler_t> -> Ptr <cef_browser_t> -> IO <void>





#starttype cef_frame_t
#field base,          <cef_base_t>
#field is_valid,      <cb_cef_frame_is_valid>
#field undo,          <cb_cef_frame_undo>
#field redo,          <cb_cef_frame_redo>
#field cut,           <cb_cef_frame_cut>
#field copy,          <cb_cef_frame_copy>
#field paste,         <cb_cef_frame_paste>
#field del,           <cb_cef_frame_del>
#field select_all,    <cb_cef_frame_select_all>
#field view_source,   <cb_cef_frame_view_source>
#field get_source,    <cb_cef_frame_get_source>
#field get_text,      <cb_cef_frame_get_text>
#field load_request,  <cb_cef_frame_load_request>
#field load_url,      <cb_cef_frame_load_url>
#field load_string,   <cb_cef_frame_load_string>
#field execute_java_script, <cb_cef_frame_execute_java_script>
#field is_main,       <cb_cef_frame_is_main>
#field is_focused,    <cb_cef_frame_is_focused>
#field get_name,      <cb_cef_frame_get_name>
#field get_identifier,  <cb_cef_frame_get_identifier>
#field get_parent,    <cb_cef_frame_get_parent>
#field get_url,       <cb_cef_frame_get_url>
#field get_browser,   <cb_cef_frame_get_browser>
#field get_v8context, <cb_cef_frame_get_v8context>
#field visit_dom,     <cb_cef_frame_visit_dom>
#stoptype

#callback_t cb_cef_frame_is_valid,      Ptr <cef_frame_t> -> IO <int>
#callback_t cb_cef_frame_undo,          Ptr <cef_frame_t> -> IO <void>
#callback_t cb_cef_frame_redo,          Ptr <cef_frame_t> -> IO <void>
#callback_t cb_cef_frame_cut,           Ptr <cef_frame_t> -> IO <void>
#callback_t cb_cef_frame_copy,          Ptr <cef_frame_t> -> IO <void>
#callback_t cb_cef_frame_paste,         Ptr <cef_frame_t> -> IO <void>
#callback_t cb_cef_frame_del,           Ptr <cef_frame_t> -> IO <void>
#callback_t cb_cef_frame_select_all,    Ptr <cef_frame_t> -> IO <void>
#callback_t cb_cef_frame_view_source,   Ptr <cef_frame_t> -> IO <void>
#callback_t cb_cef_frame_get_source,    Ptr <cef_frame_t> -> Ptr <cef_string_visitor_t> -> IO <void>
#callback_t cb_cef_frame_get_text,      Ptr <cef_frame_t> -> Ptr <cef_string_visitor_t> -> IO <void>
#callback_t cb_cef_frame_load_request,  Ptr <cef_frame_t> -> Ptr <cef_request_t> -> IO <void>
#callback_t cb_cef_frame_load_url,      Ptr <cef_frame_t> -> Ptr <cef_string_t> -> IO <void>
#callback_t cb_cef_frame_load_string,   Ptr <cef_frame_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> IO <void>
#callback_t cb_cef_frame_execute_java_script, Ptr <cef_frame_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> <int> -> IO <void>
#callback_t cb_cef_frame_is_main,       Ptr <cef_frame_t> -> IO <int>
#callback_t cb_cef_frame_is_focused,    Ptr <cef_frame_t> -> IO <int>
#callback_t cb_cef_frame_get_name,      Ptr <cef_frame_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_frame_get_identifier,  Ptr <cef_frame_t> -> IO <int64>
#callback_t cb_cef_frame_get_parent,    Ptr <cef_frame_t> -> IO (Ptr <cef_frame_t>)
#callback_t cb_cef_frame_get_url,       Ptr <cef_frame_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_frame_get_browser,   Ptr <cef_frame_t> -> IO (Ptr <cef_browser_t>)
#callback_t cb_cef_frame_get_v8context, Ptr <cef_frame_t> -> IO (Ptr <cef_v8context_t>)
#callback_t cb_cef_frame_visit_dom,     Ptr <cef_frame_t> -> Ptr <cef_domvisitor_t> -> IO <void>






#starttype cef_get_geolocation_callback_t
#field base,                <cef_base_t>
#field on_location_update,  <cb_cef_get_geolocation_callback_on_location_update>
#stoptype

#callback_t cb_cef_get_geolocation_callback_on_location_update,  Ptr <cef_get_geolocation_callback_t> -> Ptr <cef_geoposition_t> -> IO <void>



#ccall cef_get_geolocation, Ptr <cef_get_geolocation_callback_t> -> IO <int>



#starttype cef_geolocation_callback_t
#field base,  <cef_base_t>
#field cont,  <cb_cef_geolocation_callback_cont>
#stoptype

#callback_t cb_cef_geolocation_callback_cont,  Ptr <cef_geolocation_callback_t> -> <int> -> IO <void>


#starttype cef_geolocation_handler_t
#field base,                              <cef_base_t>
#field on_request_geolocation_permission, <cb_cef_geolocation_handler_on_request_geolocation_permission>
#field on_cancel_geolocation_permission,  <cb_cef_geolocation_handler_on_cancel_geolocation_permission>
#stoptype

#callback_t cb_cef_geolocation_handler_on_request_geolocation_permission, Ptr <cef_geolocation_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_string_t> -> <int> -> Ptr <cef_geolocation_callback_t> -> IO <void>
#callback_t cb_cef_geolocation_handler_on_cancel_geolocation_permission,  Ptr <cef_geolocation_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_string_t> -> <int> -> IO <void>





#starttype cef_jsdialog_callback_t
#field base,  <cef_base_t>
#field cont,  <cb_cef_jsdialog_callback_cont>
#stoptype

#callback_t cb_cef_jsdialog_callback_cont,  Ptr <cef_jsdialog_callback_t> -> <int> -> Ptr <cef_string_t> -> IO <void>


#starttype cef_jsdialog_handler_t
#field base,                <cef_base_t>
#field on_jsdialog,         <cb_cef_jsdialog_handler_on_jsdialog>
#field on_before_unload_dialog, <cb_cef_jsdialog_handler_on_before_unload_dialog>
#field on_reset_dialog_state,   <cb_cef_jsdialog_handler_on_reset_dialog_state>
#field on_dialog_closed,        <cb_cef_jsdialog_handler_on_dialog_closed>
#stoptype

#callback_t cb_cef_jsdialog_handler_on_jsdialog,         Ptr <cef_jsdialog_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> <cef_jsdialog_type_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> Ptr <cef_jsdialog_callback_t> -> Ptr <int> -> IO <int>
#callback_t cb_cef_jsdialog_handler_on_before_unload_dialog, Ptr <cef_jsdialog_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_string_t> -> <int> -> Ptr <cef_jsdialog_callback_t> -> IO <int>
#callback_t cb_cef_jsdialog_handler_on_reset_dialog_state,   Ptr <cef_jsdialog_handler_t> -> Ptr <cef_browser_t> -> IO <void>
#callback_t cb_cef_jsdialog_handler_on_dialog_closed,        Ptr <cef_jsdialog_handler_t> -> Ptr <cef_browser_t> -> IO <void>






#starttype cef_keyboard_handler_t
#field base,              <cef_base_t>
#field on_pre_key_event,  <cb_cef_keyboard_handler_on_pre_key_event>
#field on_key_event,      <cb_cef_keyboard_handler_on_key_event>
#stoptype

#callback_t cb_cef_keyboard_handler_on_pre_key_event,  Ptr <cef_keyboard_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_key_event_t> -> <cef_event_handle_t> -> Ptr <int> -> IO <int>
#callback_t cb_cef_keyboard_handler_on_key_event,      Ptr <cef_keyboard_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_key_event_t> -> <cef_event_handle_t> -> IO <int>






#starttype cef_life_span_handler_t
#field base,                <cef_base_t>
#field on_before_popup,     <cb_cef_life_span_handler_on_before_popup>
#field on_after_created,    <cb_cef_life_span_handler_on_after_created>
#field run_modal,           <cb_cef_life_span_handler_run_modal>
#field do_close,            <cb_cef_life_span_handler_do_close>
#field on_before_close,     <cb_cef_life_span_handler_on_before_close>
#stoptype

#callback_t cb_cef_life_span_handler_on_before_popup,     Ptr <cef_life_span_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> Ptr <cef_popup_features_t> -> Ptr <cef_window_info_t> -> Ptr (Ptr <cef_client_t>) -> Ptr <cef_browser_settings_t> -> Ptr <int> -> IO <int>
#callback_t cb_cef_life_span_handler_on_after_created,    Ptr <cef_life_span_handler_t> -> Ptr <cef_browser_t> -> IO <void>
#callback_t cb_cef_life_span_handler_run_modal,           Ptr <cef_life_span_handler_t> -> Ptr <cef_browser_t> -> IO <int>
#callback_t cb_cef_life_span_handler_do_close,            Ptr <cef_life_span_handler_t> -> Ptr <cef_browser_t> -> IO <int>
#callback_t cb_cef_life_span_handler_on_before_close,     Ptr <cef_life_span_handler_t> -> Ptr <cef_browser_t> -> IO <void>






#starttype cef_load_handler_t
#field base,                      <cef_base_t>
#field on_loading_state_change,   <cb_cef_load_handler_on_loading_state_change>
#field on_load_start,             <cb_cef_load_handler_on_load_start>
#field on_load_end,               <cb_cef_load_handler_on_load_end>
#field on_load_error,             <cb_cef_load_handler_on_load_error>
#stoptype

#callback_t cb_cef_load_handler_on_loading_state_change,   Ptr <cef_load_handler_t> -> Ptr <cef_browser_t> -> <int> -> <int> -> <int> -> IO <void>
#callback_t cb_cef_load_handler_on_load_start,             Ptr <cef_load_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> IO <void>
#callback_t cb_cef_load_handler_on_load_end,               Ptr <cef_load_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> <int> -> IO <void>
#callback_t cb_cef_load_handler_on_load_error,             Ptr <cef_load_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> <cef_errorcode_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> IO <void>






#starttype cef_menu_model_t
#field base,                  <cef_base_t>
#field clear,                 <cb_cef_menu_model_clear>
#field get_count,             <cb_cef_menu_model_get_count>
#field add_separator,         <cb_cef_menu_model_add_separator>
#field add_item,              <cb_cef_menu_model_add_item>
#field add_check_item,        <cb_cef_menu_model_add_check_item>
#field add_radio_item,        <cb_cef_menu_model_add_radio_item>
#field add_sub_menu,          <cb_cef_menu_model_add_sub_menu>
#field insert_separator_at,   <cb_cef_menu_model_insert_separator_at>
#field insert_item_at,        <cb_cef_menu_model_insert_item_at>
#field insert_check_item_at,  <cb_cef_menu_model_insert_check_item_at>
#field insert_radio_item_at,  <cb_cef_menu_model_insert_radio_item_at>
#field insert_sub_menu_at,    <cb_cef_menu_model_insert_sub_menu_at>
#field remove,                <cb_cef_menu_model_remove>
#field remove_at,             <cb_cef_menu_model_remove_at>
#field get_index_of,          <cb_cef_menu_model_get_index_of>
#field get_command_id_at,     <cb_cef_menu_model_get_command_id_at>
#field set_command_id_at,     <cb_cef_menu_model_set_command_id_at>
#field get_label,             <cb_cef_menu_model_get_label>
#field get_label_at,          <cb_cef_menu_model_get_label_at>
#field set_label,             <cb_cef_menu_model_set_label>
#field set_label_at,          <cb_cef_menu_model_set_label_at>
#field get_type,              <cb_cef_menu_model_get_type>
#field get_type_at,           <cb_cef_menu_model_get_type_at>
#field get_group_id,          <cb_cef_menu_model_get_group_id>
#field get_group_id_at,       <cb_cef_menu_model_get_group_id_at>
#field set_group_id,          <cb_cef_menu_model_set_group_id>
#field set_group_id_at,       <cb_cef_menu_model_set_group_id_at>
#field get_sub_menu,          <cb_cef_menu_model_get_sub_menu>
#field get_sub_menu_at,       <cb_cef_menu_model_get_sub_menu_at>
#field is_visible,            <cb_cef_menu_model_is_visible>
#field is_visible_at,         <cb_cef_menu_model_is_visible_at>
#field set_visible,           <cb_cef_menu_model_set_visible>
#field set_visible_at,        <cb_cef_menu_model_set_visible_at>
#field is_enabled,            <cb_cef_menu_model_is_enabled>
#field is_enabled_at,         <cb_cef_menu_model_is_enabled_at>
#field set_enabled,           <cb_cef_menu_model_set_enabled>
#field set_enabled_at,        <cb_cef_menu_model_set_enabled_at>
#field is_checked,            <cb_cef_menu_model_is_checked>
#field is_checked_at,         <cb_cef_menu_model_is_checked_at>
#field set_checked,           <cb_cef_menu_model_set_checked>
#field set_checked_at,        <cb_cef_menu_model_set_checked_at>
#field has_accelerator,       <cb_cef_menu_model_has_accelerator>
#field has_accelerator_at,    <cb_cef_menu_model_has_accelerator_at>
#field set_accelerator,       <cb_cef_menu_model_set_accelerator>
#field set_accelerator_at,    <cb_cef_menu_model_set_accelerator_at>
#field remove_accelerator,    <cb_cef_menu_model_remove_accelerator>
#field remove_accelerator_at, <cb_cef_menu_model_remove_accelerator_at>
#field get_accelerator,       <cb_cef_menu_model_get_accelerator>
#field get_accelerator_at,    <cb_cef_menu_model_get_accelerator_at>
#stoptype

#callback_t cb_cef_menu_model_clear,                 Ptr <cef_menu_model_t> -> IO <int>
#callback_t cb_cef_menu_model_get_count,             Ptr <cef_menu_model_t> -> IO <int>
#callback_t cb_cef_menu_model_add_separator,         Ptr <cef_menu_model_t> -> IO <int>
#callback_t cb_cef_menu_model_add_item,              Ptr <cef_menu_model_t> -> <int> -> Ptr <cef_string_t> -> IO <int>
#callback_t cb_cef_menu_model_add_check_item,        Ptr <cef_menu_model_t> -> <int> -> Ptr <cef_string_t> -> IO <int>
#callback_t cb_cef_menu_model_add_radio_item,        Ptr <cef_menu_model_t> -> <int> -> Ptr <cef_string_t> -> <int> -> IO <int>
#callback_t cb_cef_menu_model_add_sub_menu,          Ptr <cef_menu_model_t> -> <int> -> Ptr <cef_string_t> -> IO (Ptr <cef_menu_model_t>)
#callback_t cb_cef_menu_model_insert_separator_at,   Ptr <cef_menu_model_t> -> <int> -> IO <int>
#callback_t cb_cef_menu_model_insert_item_at,        Ptr <cef_menu_model_t> -> <int> -> <int> -> Ptr <cef_string_t> -> IO <int>
#callback_t cb_cef_menu_model_insert_check_item_at,  Ptr <cef_menu_model_t> -> <int> -> <int> -> Ptr <cef_string_t> -> IO <int>
#callback_t cb_cef_menu_model_insert_radio_item_at,  Ptr <cef_menu_model_t> -> <int> -> <int> -> Ptr <cef_string_t> -> <int> -> IO <int>
#callback_t cb_cef_menu_model_insert_sub_menu_at,    Ptr <cef_menu_model_t> -> <int> -> <int> -> Ptr <cef_string_t> -> IO (Ptr <cef_menu_model_t>)
#callback_t cb_cef_menu_model_remove,                Ptr <cef_menu_model_t> -> <int> -> IO <int>
#callback_t cb_cef_menu_model_remove_at,             Ptr <cef_menu_model_t> -> <int> -> IO <int>
#callback_t cb_cef_menu_model_get_index_of,          Ptr <cef_menu_model_t> -> <int> -> IO <int>
#callback_t cb_cef_menu_model_get_command_id_at,     Ptr <cef_menu_model_t> -> <int> -> IO <int>
#callback_t cb_cef_menu_model_set_command_id_at,     Ptr <cef_menu_model_t> -> <int> -> <int> -> IO <int>
#callback_t cb_cef_menu_model_get_label,             Ptr <cef_menu_model_t> -> <int> -> IO <cef_string_userfree_t>
#callback_t cb_cef_menu_model_get_label_at,          Ptr <cef_menu_model_t> -> <int> -> IO <cef_string_userfree_t>
#callback_t cb_cef_menu_model_set_label,             Ptr <cef_menu_model_t> -> <int> -> Ptr <cef_string_t> -> IO <int>
#callback_t cb_cef_menu_model_set_label_at,          Ptr <cef_menu_model_t> -> <int> -> Ptr <cef_string_t> -> IO <int>
#callback_t cb_cef_menu_model_get_type,              Ptr <cef_menu_model_t> -> <int> -> IO <cef_menu_item_type_t>
#callback_t cb_cef_menu_model_get_type_at,           Ptr <cef_menu_model_t> -> <int> -> IO <cef_menu_item_type_t>
#callback_t cb_cef_menu_model_get_group_id,          Ptr <cef_menu_model_t> -> <int> -> IO <int>
#callback_t cb_cef_menu_model_get_group_id_at,       Ptr <cef_menu_model_t> -> <int> -> IO <int>
#callback_t cb_cef_menu_model_set_group_id,          Ptr <cef_menu_model_t> -> <int> -> <int> -> IO <int>
#callback_t cb_cef_menu_model_set_group_id_at,       Ptr <cef_menu_model_t> -> <int> -> <int> -> IO <int>
#callback_t cb_cef_menu_model_get_sub_menu,          Ptr <cef_menu_model_t> -> <int> -> IO (Ptr <cef_menu_model_t>)
#callback_t cb_cef_menu_model_get_sub_menu_at,       Ptr <cef_menu_model_t> -> <int> -> IO (Ptr <cef_menu_model_t>)
#callback_t cb_cef_menu_model_is_visible,            Ptr <cef_menu_model_t> -> <int> -> IO <int>
#callback_t cb_cef_menu_model_is_visible_at,         Ptr <cef_menu_model_t> -> <int> -> IO <int>
#callback_t cb_cef_menu_model_set_visible,           Ptr <cef_menu_model_t> -> <int> -> <int> -> IO <int>
#callback_t cb_cef_menu_model_set_visible_at,        Ptr <cef_menu_model_t> -> <int> -> <int> -> IO <int>
#callback_t cb_cef_menu_model_is_enabled,            Ptr <cef_menu_model_t> -> <int> -> IO <int>
#callback_t cb_cef_menu_model_is_enabled_at,         Ptr <cef_menu_model_t> -> <int> -> IO <int>
#callback_t cb_cef_menu_model_set_enabled,           Ptr <cef_menu_model_t> -> <int> -> <int> -> IO <int>
#callback_t cb_cef_menu_model_set_enabled_at,        Ptr <cef_menu_model_t> -> <int> -> <int> -> IO <int>
#callback_t cb_cef_menu_model_is_checked,            Ptr <cef_menu_model_t> -> <int> -> IO <int>
#callback_t cb_cef_menu_model_is_checked_at,         Ptr <cef_menu_model_t> -> <int> -> IO <int>
#callback_t cb_cef_menu_model_set_checked,           Ptr <cef_menu_model_t> -> <int> -> <int> -> IO <int>
#callback_t cb_cef_menu_model_set_checked_at,        Ptr <cef_menu_model_t> -> <int> -> <int> -> IO <int>
#callback_t cb_cef_menu_model_has_accelerator,       Ptr <cef_menu_model_t> -> <int> -> IO <int>
#callback_t cb_cef_menu_model_has_accelerator_at,    Ptr <cef_menu_model_t> -> <int> -> IO <int>
#callback_t cb_cef_menu_model_set_accelerator,       Ptr <cef_menu_model_t> -> <int> -> <int> -> <int> -> <int> -> <int> -> IO <int>
#callback_t cb_cef_menu_model_set_accelerator_at,    Ptr <cef_menu_model_t> -> <int> -> <int> -> <int> -> <int> -> <int> -> IO <int>
#callback_t cb_cef_menu_model_remove_accelerator,    Ptr <cef_menu_model_t> -> <int> -> IO <int>
#callback_t cb_cef_menu_model_remove_accelerator_at, Ptr <cef_menu_model_t> -> <int> -> IO <int>
#callback_t cb_cef_menu_model_get_accelerator,       Ptr <cef_menu_model_t> -> <int> -> Ptr <int> -> Ptr <int> -> Ptr <int> -> Ptr <int> -> IO <int>
#callback_t cb_cef_menu_model_get_accelerator_at,    Ptr <cef_menu_model_t> -> <int> -> Ptr <int> -> Ptr <int> -> Ptr <int> -> Ptr <int> -> IO <int>





#ccall cef_add_cross_origin_whitelist_entry,    Ptr <cef_string_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> <int> -> IO <int>
#ccall cef_remove_cross_origin_whitelist_entry, Ptr <cef_string_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> <int> -> IO <int>
#ccall cef_clear_cross_origin_whitelist,        IO <int>




#ccall cef_get_path, <cef_path_key_t> -> Ptr <cef_string_t> -> IO <int>




#starttype cef_process_message_t
#field base,                <cef_base_t>
#field is_valid,            <cb_cef_process_message_is_valid>
#field is_read_only,        <cb_cef_process_message_is_read_only>
#field copy,                <cb_cef_process_message_copy>
#field get_name,            <cb_cef_process_message_get_name>
#field get_argument_list,   <cb_cef_process_message_get_argument_list>
#stoptype

#callback_t cb_cef_process_message_is_valid,            Ptr <cef_process_message_t> -> IO <int>
#callback_t cb_cef_process_message_is_read_only,        Ptr <cef_process_message_t> -> IO <int>
#callback_t cb_cef_process_message_copy,                Ptr <cef_process_message_t> -> IO (Ptr <cef_process_message_t>)
#callback_t cb_cef_process_message_get_name,            Ptr <cef_process_message_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_process_message_get_argument_list,   Ptr <cef_process_message_t> -> IO (Ptr <cef_list_value_t>)


#ccall cef_process_message_create, Ptr <cef_string_t> -> IO (Ptr <cef_process_message_t>)





#ccall cef_launch_process, Ptr <cef_command_line_t> -> IO <int>





#starttype cef_render_handler_t
#field base,                  <cef_base_t>
#field get_root_screen_rect,  <cb_cef_render_handler_get_root_screen_rect>
#field get_view_rect,         <cb_cef_render_handler_get_view_rect>
#field get_screen_point,      <cb_cef_render_handler_get_screen_point>
#field get_screen_info,       <cb_cef_render_handler_get_screen_info>
#field on_popup_show,         <cb_cef_render_handler_on_popup_show>
#field on_popup_size,         <cb_cef_render_handler_on_popup_size>
#field on_paint,              <cb_cef_render_handler_on_paint>
#field on_cursor_change,      <cb_cef_render_handler_on_cursor_change>
#field on_scroll_offset_changed, <cb_cef_render_handler_on_scroll_offset_changed>
#stoptype

#callback_t cb_cef_render_handler_get_root_screen_rect,  Ptr <cef_render_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_rect_t> -> IO <int>
#callback_t cb_cef_render_handler_get_view_rect,         Ptr <cef_render_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_rect_t> -> IO <int>
#callback_t cb_cef_render_handler_get_screen_point,      Ptr <cef_render_handler_t> -> Ptr <cef_browser_t> -> <int> -> <int> -> Ptr <int> -> Ptr <int> -> IO <int>
#callback_t cb_cef_render_handler_get_screen_info,       Ptr <cef_render_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_screen_info_t> -> IO <int>
#callback_t cb_cef_render_handler_on_popup_show,         Ptr <cef_render_handler_t> -> Ptr <cef_browser_t> -> <int> -> IO <void>
#callback_t cb_cef_render_handler_on_popup_size,         Ptr <cef_render_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_rect_t> -> IO <void>
#callback_t cb_cef_render_handler_on_paint,              Ptr <cef_render_handler_t> -> Ptr <cef_browser_t> -> <cef_paint_element_type_t> -> <size_t> -> Ptr <cef_rect_t> -> Ptr <void> -> <int> -> <int> -> IO <void>
#callback_t cb_cef_render_handler_on_cursor_change,      Ptr <cef_render_handler_t> -> Ptr <cef_browser_t> -> <cef_cursor_handle_t> -> IO <void>
#callback_t cb_cef_render_handler_on_scroll_offset_changed, Ptr <cef_render_handler_t> -> Ptr <cef_browser_t> -> IO <void>





#starttype cef_render_process_handler_t
#field base,                        <cef_base_t>
#field on_render_thread_created,    <cb_cef_render_process_handler_on_render_thread_created>
#field on_web_kit_initialized,      <cb_cef_render_process_handler_on_web_kit_initialized>
#field on_browser_created,          <cb_cef_render_process_handler_on_browser_created>
#field on_browser_destroyed,        <cb_cef_render_process_handler_on_browser_destroyed>
#field get_load_handler,            <cb_cef_render_process_handler_get_load_handler>
#field on_before_navigation,        <cb_cef_render_process_handler_on_before_navigation>
#field on_context_created,          <cb_cef_render_process_handler_on_context_created>
#field on_context_released,         <cb_cef_render_process_handler_on_context_released>
#field on_uncaught_exception,       <cb_cef_render_process_handler_on_uncaught_exception>
#field on_focused_node_changed,     <cb_cef_render_process_handler_on_focused_node_changed>
#field on_process_message_received, <cb_cef_render_process_handler_on_process_message_received>
#stoptype

#callback_t cb_cef_render_process_handler_on_render_thread_created,    Ptr <cef_render_process_handler_t> -> Ptr <cef_list_value_t> -> IO <void>
#callback_t cb_cef_render_process_handler_on_web_kit_initialized,      Ptr <cef_render_process_handler_t> -> IO <void>
#callback_t cb_cef_render_process_handler_on_browser_created,          Ptr <cef_render_process_handler_t> -> Ptr <cef_browser_t> -> IO <void>
#callback_t cb_cef_render_process_handler_on_browser_destroyed,        Ptr <cef_render_process_handler_t> -> Ptr <cef_browser_t> -> IO <void>
#callback_t cb_cef_render_process_handler_get_load_handler,            Ptr <cef_render_process_handler_t> -> IO (Ptr <cef_load_handler_t>)
#callback_t cb_cef_render_process_handler_on_before_navigation,        Ptr <cef_render_process_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> Ptr <cef_request_t> -> <cef_navigation_type_t> -> <int> -> IO <int>
#callback_t cb_cef_render_process_handler_on_context_created,          Ptr <cef_render_process_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> Ptr <cef_v8context_t> -> IO <void>
#callback_t cb_cef_render_process_handler_on_context_released,         Ptr <cef_render_process_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> Ptr <cef_v8context_t> -> IO <void>
#callback_t cb_cef_render_process_handler_on_uncaught_exception,       Ptr <cef_render_process_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> Ptr <cef_v8context_t> -> Ptr <cef_v8exception_t> -> Ptr <cef_v8stack_trace_t> -> IO <void>
#callback_t cb_cef_render_process_handler_on_focused_node_changed,     Ptr <cef_render_process_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> Ptr <cef_domnode_t> -> IO <void>
#callback_t cb_cef_render_process_handler_on_process_message_received, Ptr <cef_render_process_handler_t> -> Ptr <cef_browser_t> -> <cef_process_id_t> -> Ptr <cef_process_message_t> -> IO <int>






#starttype cef_request_context_t
#field base,        <cef_base_t>
#field is_same,     <cb_cef_request_context_is_same>
#field is_global,   <cb_cef_request_context_is_global>
#field get_handler, <cb_cef_request_context_get_handler>
#stoptype

#callback_t cb_cef_request_context_is_same,     Ptr <cef_request_context_t> -> Ptr <cef_request_context_t> -> IO <int>
#callback_t cb_cef_request_context_is_global,   Ptr <cef_request_context_t> -> IO <int>
#callback_t cb_cef_request_context_get_handler, Ptr <cef_request_context_t> -> IO (Ptr <cef_request_context_handler_t>)


#ccall cef_request_create, IO (Ptr <cef_request_t>)
#ccall cef_post_data_create, IO (Ptr <cef_post_data_t>)
#ccall cef_post_data_element_create, IO (Ptr <cef_post_data_element_t>)




#starttype cef_request_context_handler_t
#field base,                <cef_base_t>
#field get_cookie_manager,  <cb_cef_request_context_handler_get_cookie_manager>
#stoptype

#callback_t cb_cef_request_context_handler_get_cookie_manager,  Ptr <cef_request_context_handler_t> -> IO (Ptr <cef_cookie_manager_t>)


#ccall cef_request_context_get_global_context,  IO (Ptr <cef_request_context_t>)
#ccall cef_request_context_create_context,      IO (Ptr <cef_request_context_t>) 





#starttype cef_quota_callback_t
#field base,    <cef_base_t>
#field cont,    <cb_cef_quota_callback_cont>
#field cancel,  <cb_cef_quota_callback_cancel>
#stoptype

#callback_t cb_cef_quota_callback_cont,    Ptr <cef_quota_callback_t> -> <int> -> IO <void>
#callback_t cb_cef_quota_callback_cancel,  Ptr <cef_quota_callback_t> -> IO <void>



#starttype cef_allow_certificate_error_callback_t
#field base,    <cef_base_t>
#field cont,    <cb_cef_allow_certificate_error_callback_cont>
#stoptype

#callback_t cb_cef_allow_certificate_error_callback_cont,    Ptr <cef_allow_certificate_error_callback_t> -> <int> -> IO <void>



#starttype cef_request_handler_t
#field base,                    <cef_base_t>
#field on_before_browse,        <cb_cef_request_handler_on_before_browse>
#field on_before_resource_load, <cb_cef_request_handler_on_before_resource_load>
#field get_resource_handler,    <cb_cef_request_handler_get_resource_handler>
#field on_resource_redirect,    <cb_cef_request_handler_on_resource_redirect>
#field get_auth_credentials,    <cb_cef_request_handler_get_auth_credentials>
#field on_quota_request,        <cb_cef_request_handler_on_quota_request>
#field on_protocol_execution,   <cb_cef_request_handler_on_protocol_execution>
#field on_certificate_error,    <cb_cef_request_handler_on_certificate_error>
#field on_before_plugin_load,   <cb_cef_request_handler_on_before_plugin_load>
#field on_plugin_crashed,       <cb_cef_request_handler_on_plugin_crashed>
#field on_render_process_terminated, <cb_cef_request_handler_on_render_process_terminated>
#stoptype

#callback_t cb_cef_request_handler_on_before_browse,        Ptr <cef_request_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> Ptr <cef_request_t> -> <int> -> IO <int>
#callback_t cb_cef_request_handler_on_before_resource_load, Ptr <cef_request_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> Ptr <cef_request_t> -> IO <int>
#callback_t cb_cef_request_handler_get_resource_handler,    Ptr <cef_request_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> Ptr <cef_request_t> -> IO (Ptr <cef_resource_handler_t>)
#callback_t cb_cef_request_handler_on_resource_redirect,    Ptr <cef_request_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> IO <void>
#callback_t cb_cef_request_handler_get_auth_credentials,    Ptr <cef_request_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> <int> -> Ptr <cef_string_t> -> <int> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> Ptr <cef_auth_callback_t> -> IO <int>
#callback_t cb_cef_request_handler_on_quota_request,        Ptr <cef_request_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_string_t> -> <int64> -> Ptr <cef_quota_callback_t> -> IO <int>
#callback_t cb_cef_request_handler_on_protocol_execution,   Ptr <cef_request_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_string_t> -> Ptr <int> -> IO <void>
#callback_t cb_cef_request_handler_on_certificate_error,    Ptr <cef_request_handler_t> -> <cef_errorcode_t> -> Ptr <cef_string_t> -> Ptr <cef_allow_certificate_error_callback_t> -> IO <int>
#callback_t cb_cef_request_handler_on_before_plugin_load,   Ptr <cef_request_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> Ptr <cef_web_plugin_info_t> -> IO <int>
#callback_t cb_cef_request_handler_on_plugin_crashed,       Ptr <cef_request_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_string_t> -> IO <void>
#callback_t cb_cef_request_handler_on_render_process_terminated, Ptr <cef_request_handler_t> -> Ptr <cef_browser_t> -> <cef_termination_status_t> -> IO <void>





#starttype cef_request_t
#field base,                <cef_base_t>
#field is_read_only,        <cb_cef_request_is_read_only>
#field get_url,             <cb_cef_request_get_url>
#field set_url,             <cb_cef_request_set_url>
#field get_method,          <cb_cef_request_get_method>
#field set_method,          <cb_cef_request_set_method>
#field get_post_data,       <cb_cef_request_get_post_data>
#field set_post_data,       <cb_cef_request_set_post_data>
#field get_header_map,      <cb_cef_request_get_header_map>
#field set_header_map,      <cb_cef_request_set_header_map>
#field set,                 <cb_cef_request_set>
#field get_flags,           <cb_cef_request_get_flags>
#field set_flags,           <cb_cef_request_set_flags>
#field get_first_party_for_cookies, <cb_cef_request_get_first_party_for_cookies>
#field set_first_party_for_cookies, <cb_cef_request_set_first_party_for_cookies>
#field get_resource_type,   <cb_cef_request_get_resource_type>
#field get_transition_type, <cb_cef_request_get_transition_type>
#stoptype

#callback_t cb_cef_request_is_read_only,        Ptr <cef_request_t> -> IO <int>
#callback_t cb_cef_request_get_url,             Ptr <cef_request_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_request_set_url,             Ptr <cef_request_t> -> Ptr <cef_string_t> -> IO <void>
#callback_t cb_cef_request_get_method,          Ptr <cef_request_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_request_set_method,          Ptr <cef_request_t> -> Ptr <cef_string_t> -> IO <void>
#callback_t cb_cef_request_get_post_data,       Ptr <cef_request_t> -> IO (Ptr <cef_post_data_t>)
#callback_t cb_cef_request_set_post_data,       Ptr <cef_request_t> -> Ptr <cef_post_data_t> -> IO <void>
#callback_t cb_cef_request_get_header_map,      Ptr <cef_request_t> -> <cef_string_multimap_t> -> IO <void>
#callback_t cb_cef_request_set_header_map,      Ptr <cef_request_t> -> <cef_string_multimap_t> -> IO <void>
#callback_t cb_cef_request_set,                 Ptr <cef_request_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> Ptr <cef_post_data_t> -> <cef_string_multimap_t> -> IO <void>
#callback_t cb_cef_request_get_flags,           Ptr <cef_request_t> -> IO <int>
#callback_t cb_cef_request_set_flags,           Ptr <cef_request_t> -> <int> -> IO <void>
#callback_t cb_cef_request_get_first_party_for_cookies, Ptr <cef_request_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_request_set_first_party_for_cookies, Ptr <cef_request_t> -> Ptr <cef_string_userfree_t> -> IO <void>
#callback_t cb_cef_request_get_resource_type,   Ptr <cef_request_t> -> IO <cef_resource_type_t>
#callback_t cb_cef_request_get_transition_type, Ptr <cef_request_t> -> IO <cef_transition_type_t>


#starttype cef_post_data_t
#field base,                <cef_base_t>
#field is_read_only,        <cb_cef_post_data_is_read_only>
#field get_element_count,   <cb_cef_post_data_get_element_count>
#field get_elements,        <cb_cef_post_data_get_elements>
#field remove_element,      <cb_cef_post_data_remove_element>
#field add_element,         <cb_cef_post_data_add_element>
#field remove_elements,     <cb_cef_post_data_remove_elements>
#stoptype

#callback_t cb_cef_post_data_is_read_only,        Ptr <cef_post_data_t> -> IO <int>
#callback_t cb_cef_post_data_get_element_count,   Ptr <cef_post_data_t> -> IO <size_t>
#callback_t cb_cef_post_data_get_elements,        Ptr <cef_post_data_t> -> Ptr <size_t> -> Ptr (Ptr <cef_post_data_element_t>) -> IO <void>
#callback_t cb_cef_post_data_remove_element,      Ptr <cef_post_data_t> -> Ptr <cef_post_data_element_t> -> IO <int>
#callback_t cb_cef_post_data_add_element,         Ptr <cef_post_data_t> -> Ptr <cef_post_data_element_t> -> IO <int>
#callback_t cb_cef_post_data_remove_elements,     Ptr <cef_post_data_t> -> IO <void>


#starttype cef_post_data_element_t
#field base,                <cef_base_t>
#field is_read_only,        <cb_cef_post_data_element_is_read_only>
#field set_to_empty,        <cb_cef_post_data_element_set_to_empty>
#field set_to_file,         <cb_cef_post_data_element_set_to_file>
#field set_to_bytes,        <cb_cef_post_data_element_set_to_bytes>
#field get_type,            <cb_cef_post_data_element_get_type>
#field get_file,            <cb_cef_post_data_element_get_file>
#field get_bytes_count,     <cb_cef_post_data_element_get_bytes_count>
#field get_bytes,           <cb_cef_post_data_element_get_bytes>
#stoptype

#callback_t cb_cef_post_data_element_is_read_only,        Ptr <cef_post_data_element_t> -> IO <int>
#callback_t cb_cef_post_data_element_set_to_empty,        Ptr <cef_post_data_element_t> -> IO <void>
#callback_t cb_cef_post_data_element_set_to_file,         Ptr <cef_post_data_element_t> -> Ptr <cef_string_t> -> IO <void>
#callback_t cb_cef_post_data_element_set_to_bytes,        Ptr <cef_post_data_element_t> -> <size_t> -> Ptr <void> -> IO <void>
#callback_t cb_cef_post_data_element_get_type,            Ptr <cef_post_data_element_t> -> IO <cef_postdataelement_type_t>
#callback_t cb_cef_post_data_element_get_file,            Ptr <cef_post_data_element_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_post_data_element_get_bytes_count,     Ptr <cef_post_data_element_t> -> IO <size_t>
#callback_t cb_cef_post_data_element_get_bytes,           Ptr <cef_post_data_element_t> -> <size_t> -> Ptr <void> -> IO <size_t>





#starttype cef_resource_bundle_handler_t
#field base,                  <cef_base_t>
#field get_localized_string,  <cb_cef_resource_bundle_handler_get_localized_string>
#field get_data_resource,     <cb_cef_resource_bundle_handler_get_data_resource>
#stoptype

#callback_t cb_cef_resource_bundle_handler_get_localized_string,  Ptr <cef_resource_bundle_handler_t> -> <int> -> Ptr <cef_string_t> -> IO <int>
#callback_t cb_cef_resource_bundle_handler_get_data_resource,     Ptr <cef_resource_bundle_handler_t> -> <int> -> Ptr (Ptr <void>) -> Ptr <size_t> -> IO <int>





#starttype cef_resource_handler_t
#field base,                      <cef_base_t>
#field process_request,           <cb_cef_resource_handler_process_request>
#field get_response_headers,      <cb_cef_resource_handler_get_response_headers>
#field read_response,             <cb_cef_resource_handler_read_response>
#field can_get_cookie,            <cb_cef_resource_handler_can_get_cookie>
#field can_set_cookie,            <cb_cef_resource_handler_can_set_cookie>
#field cancel,                    <cb_cef_resource_handler_cancel>
#stoptype

#callback_t cb_cef_resource_handler_process_request,           Ptr <cef_resource_handler_t> -> Ptr <cef_request_t> -> Ptr <cef_callback_t> -> IO <int>
#callback_t cb_cef_resource_handler_get_response_headers,      Ptr <cef_resource_handler_t> -> Ptr <cef_response_t> -> Ptr <int64> -> Ptr <cef_string_t> -> IO <void>
#callback_t cb_cef_resource_handler_read_response,             Ptr <cef_resource_handler_t> -> Ptr <void> -> <int> -> Ptr <int> -> Ptr <cef_callback_t> -> IO <int>
#callback_t cb_cef_resource_handler_can_get_cookie,            Ptr <cef_resource_handler_t> -> Ptr <cef_cookie_t> -> IO <int>
#callback_t cb_cef_resource_handler_can_set_cookie,            Ptr <cef_resource_handler_t> -> Ptr <cef_cookie_t> -> IO <int>
#callback_t cb_cef_resource_handler_cancel,                    Ptr <cef_resource_handler_t> -> IO <void>






#starttype cef_response_t
#field base,                <cef_base_t>
#field is_read_only,        <cb_cef_response_is_read_only>
#field get_status,          <cb_cef_response_get_status>
#field set_status,          <cb_cef_response_set_status>
#field get_status_text,     <cb_cef_response_get_status_text>
#field set_status_text,     <cb_cef_response_set_status_text>
#field get_mime_type,       <cb_cef_response_get_mime_type>
#field set_mime_type,       <cb_cef_response_set_mime_type>
#field get_header,          <cb_cef_response_get_header>
#field get_header_map,      <cb_cef_response_get_header_map>
#field set_header_map,      <cb_cef_response_set_header_map>
#stoptype

#callback_t cb_cef_response_is_read_only,        Ptr <cef_response_t> -> IO <int>
#callback_t cb_cef_response_get_status,          Ptr <cef_response_t> -> IO <int>
#callback_t cb_cef_response_set_status,          Ptr <cef_response_t> -> <int> -> IO <void>
#callback_t cb_cef_response_get_status_text,     Ptr <cef_response_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_response_set_status_text,     Ptr <cef_response_t> -> Ptr <cef_string_t> -> IO <void>
#callback_t cb_cef_response_get_mime_type,       Ptr <cef_response_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_response_set_mime_type,       Ptr <cef_response_t> -> Ptr <cef_string_t> -> IO <void>
#callback_t cb_cef_response_get_header,          Ptr <cef_response_t> -> Ptr <cef_string_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_response_get_header_map,      Ptr <cef_response_t> -> <cef_string_multimap_t> -> IO <void>
#callback_t cb_cef_response_set_header_map,      Ptr <cef_response_t> -> <cef_string_multimap_t> -> IO <void>


#ccall cef_response_create, IO (Ptr <cef_response_t>)





#starttype cef_scheme_registrar_t
#field base,                <cef_base_t>
#field add_custom_scheme,   <cb_cef_scheme_registrar_add_custom_scheme>
#stoptype

#callback_t cb_cef_scheme_registrar_add_custom_scheme,   Ptr <cef_scheme_registrar_t> -> Ptr <cef_string_t> -> <int> -> <int> -> <int> -> IO <int>


#starttype cef_scheme_handler_factory_t
#field base,    <cef_base_t>
#field create,  <cb_cef_scheme_handler_factory_create>
#stoptype

#callback_t cb_cef_scheme_handler_factory_create,  Ptr <cef_scheme_handler_factory_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> Ptr <cef_string_t> -> Ptr <cef_request_t> -> IO (Ptr <cef_resource_handler_t>)


#ccall cef_register_scheme_handler_factory, Ptr <cef_string_t> -> Ptr <cef_string_t> -> Ptr <cef_scheme_handler_factory_t> -> IO <int>
#ccall cef_clear_scheme_handler_factories,  IO <int>






#starttype cef_read_handler_t
#field base,      <cef_base_t>
#field read,      <cb_cef_read_handler_read>
#field seek,      <cb_cef_read_handler_seek>
#field tell,      <cb_cef_read_handler_tell>
#field eof,       <cb_cef_read_handler_eof>
#field may_block, <cb_cef_read_handler_may_block>
#stoptype

#callback_t cb_cef_read_handler_read,      Ptr <cef_read_handler_t> -> Ptr <void> -> <size_t> -> <size_t> -> IO <size_t>
#callback_t cb_cef_read_handler_seek,      Ptr <cef_read_handler_t> -> <int64> -> <int> -> IO <int>
#callback_t cb_cef_read_handler_tell,      Ptr <cef_read_handler_t> -> IO <int64>
#callback_t cb_cef_read_handler_eof,       Ptr <cef_read_handler_t> -> IO <int>
#callback_t cb_cef_read_handler_may_block, Ptr <cef_read_handler_t> -> IO <int>




#starttype cef_stream_reader_t
#field base,      <cef_base_t>
#field read,      <cb_cef_stream_reader_read>
#field seek,      <cb_cef_stream_reader_seek>
#field tell,      <cb_cef_stream_reader_tell>
#field eof,       <cb_cef_stream_reader_eof>
#field may_block, <cb_cef_stream_reader_may_block>
#stoptype

#callback_t cb_cef_stream_reader_read,      Ptr <cef_stream_reader_t> -> Ptr <void> -> <size_t> -> <size_t> -> IO <size_t>
#callback_t cb_cef_stream_reader_seek,      Ptr <cef_stream_reader_t> -> <int64> -> <int> -> IO <int>
#callback_t cb_cef_stream_reader_tell,      Ptr <cef_stream_reader_t> -> IO <int64>
#callback_t cb_cef_stream_reader_eof,       Ptr <cef_stream_reader_t> -> IO <int>
#callback_t cb_cef_stream_reader_may_block, Ptr <cef_stream_reader_t> -> IO <int>




#starttype cef_write_handler_t
#field base,      <cef_base_t>
#field write,     <cb_cef_write_handler_write>
#field seek,      <cb_cef_write_handler_seek>
#field tell,      <cb_cef_write_handler_tell>
#field flush,     <cb_cef_write_handler_flush>
#field may_block, <cb_cef_write_handler_may_block>
#stoptype

#callback_t cb_cef_write_handler_write,     Ptr <cef_write_handler_t> -> Ptr <void> -> <size_t> -> <size_t> -> IO <size_t>
#callback_t cb_cef_write_handler_seek,      Ptr <cef_write_handler_t> -> <int64> -> <int> -> IO <int>
#callback_t cb_cef_write_handler_tell,      Ptr <cef_write_handler_t> -> IO <int64>
#callback_t cb_cef_write_handler_flush,     Ptr <cef_write_handler_t> -> IO <int>
#callback_t cb_cef_write_handler_may_block, Ptr <cef_write_handler_t> -> IO <int>




#starttype cef_stream_writer_t
#field base,      <cef_base_t>
#field write,     <cb_cef_stream_writer_write>
#field seek,      <cb_cef_stream_writer_seek>
#field tell,      <cb_cef_stream_writer_tell>
#field flush,     <cb_cef_stream_writer_flush>
#field may_block, <cb_cef_stream_writer_may_block>
#stoptype

#callback_t cb_cef_stream_writer_write,     Ptr <cef_stream_writer_t> -> Ptr <void> -> <size_t> -> <size_t> -> IO <size_t>
#callback_t cb_cef_stream_writer_seek,      Ptr <cef_stream_writer_t> -> <int64> -> <int> -> IO <int>
#callback_t cb_cef_stream_writer_tell,      Ptr <cef_stream_writer_t> -> IO <int64>
#callback_t cb_cef_stream_writer_flush,     Ptr <cef_stream_writer_t> -> IO <int>
#callback_t cb_cef_stream_writer_may_block, Ptr <cef_stream_writer_t> -> IO <int>


#ccall cef_stream_reader_create_for_file,     Ptr <cef_string_t> -> IO (Ptr <cef_stream_reader_t>)
#ccall cef_stream_reader_create_for_data,     Ptr <void> -> <size_t> -> IO (Ptr <cef_stream_reader_t>)
#ccall cef_stream_reader_create_for_handler,  Ptr <cef_read_handler_t> -> IO (Ptr <cef_stream_reader_t>)

#ccall cef_stream_writer_create_for_file,     Ptr <cef_string_t> -> IO (Ptr <cef_stream_writer_t>)
#ccall cef_stream_writer_create_for_handler,  Ptr <cef_write_handler_t> -> IO (Ptr <cef_stream_writer_t>)





#starttype cef_string_visitor_t
#field base,  <cef_base_t>
#field visit, <cb_cef_string_visitor_visit>
#stoptype

#callback_t cb_cef_string_visitor_visit, Ptr <cef_string_visitor_t> -> Ptr <cef_string_t> -> IO <void>






#starttype cef_task_t
#field base,    <cef_base_t>
#field execute, <cb_cef_task_execute>
#stoptype

#callback_t cb_cef_task_execute, Ptr <cef_task_t> -> IO <void>


#starttype cef_task_runner_t
#field base,                      <cef_base_t>
#field is_same,                   <cb_cef_task_runner_is_same>
#field belongs_to_current_thread, <cb_cef_task_runner_belongs_to_current_thread>
#field belongs_to_thread,         <cb_cef_task_runner_belongs_to_thread>
#field post_task,                 <cb_cef_task_runner_post_task>
#field post_delayed_task,         <cb_cef_task_runner_post_delayed_task>
#stoptype

#callback_t cb_cef_task_runner_is_same,                   Ptr <cef_task_runner_t> -> Ptr <cef_task_runner_t> -> IO <int>
#callback_t cb_cef_task_runner_belongs_to_current_thread, Ptr <cef_task_runner_t> -> IO <int>
#callback_t cb_cef_task_runner_belongs_to_thread,         Ptr <cef_task_runner_t> -> <cef_thread_id_t> -> IO <int>
#callback_t cb_cef_task_runner_post_task,                 Ptr <cef_task_runner_t> -> Ptr <cef_task_t> -> IO <int>
#callback_t cb_cef_task_runner_post_delayed_task,         Ptr <cef_task_runner_t> -> Ptr <cef_task_t> -> <int64> -> IO <int>


#ccall cef_task_runner_get_for_current_thread,  IO (Ptr <cef_task_runner_t>)
#ccall cef_task_runner_get_for_thread,          IO (Ptr <cef_task_runner_t>)
#ccall cef_currently_on,                        <cef_thread_id_t> -> IO <int>
#ccall cef_post_task,                           <cef_thread_id_t> -> Ptr <cef_task_t> -> IO <int>
#ccall cef_post_delayed_task,                   <cef_thread_id_t> -> Ptr <cef_task_t> -> <int64> -> IO <int>





#starttype cef_end_tracing_callback_t
#field base,                    <cef_base_t>
#field on_end_tracing_complete, <cb_cef_end_tracing_callback_on_end_tracing_complete>
#stoptype

#callback_t cb_cef_end_tracing_callback_on_end_tracing_complete, Ptr <cef_end_tracing_callback_t> -> Ptr <cef_string_t> -> IO <void>


#ccall cef_begin_tracing,               Ptr <cef_string_t> -> IO <int>
#ccall cef_end_tracing_async,           Ptr <cef_string_t> -> Ptr <cef_end_tracing_callback_t> -> IO <int>
#ccall cef_now_from_system_trace_time,  IO <int64>






#starttype cef_urlrequest_t
#field base,                <cef_base_t>
#field get_request,         <cb_cef_urlrequest_get_request>
#field get_client,          <cb_cef_urlrequest_get_client>
#field get_request_status,  <cb_cef_urlrequest_get_request_status>
#field get_request_error,   <cb_cef_urlrequest_get_request_error>
#field get_response,        <cb_cef_urlrequest_get_response>
#field cancel,              <cb_cef_urlrequest_cancel>
#stoptype

#callback_t cb_cef_urlrequest_get_request,         Ptr <cef_urlrequest_t> -> IO (Ptr <cef_request_t>)
#callback_t cb_cef_urlrequest_get_client,          Ptr <cef_urlrequest_t> -> IO (Ptr <cef_urlrequest_client_t>)
#callback_t cb_cef_urlrequest_get_request_status,  Ptr <cef_urlrequest_t> -> IO <cef_urlrequest_status_t>
#callback_t cb_cef_urlrequest_get_request_error,   Ptr <cef_urlrequest_t> -> IO <cef_errorcode_t>
#callback_t cb_cef_urlrequest_get_response,        Ptr <cef_urlrequest_t> -> IO (Ptr <cef_response_t>)
#callback_t cb_cef_urlrequest_cancel,              Ptr <cef_urlrequest_t> -> IO <void>


#starttype cef_urlrequest_client_t
#field base,                  <cef_base_t>
#field on_request_complete,   <cb_cef_urlrequest_client_on_request_complete>
#field on_upload_progress,    <cb_cef_urlrequest_client_on_upload_progress>
#field on_download_progress,  <cb_cef_urlrequest_client_on_download_progress>
#field on_download_data,      <cb_cef_urlrequest_client_on_download_data>
#field get_auth_credentials,  <cb_cef_urlrequest_client_get_auth_credentials>
#stoptype

#callback_t cb_cef_urlrequest_client_on_request_complete,   Ptr <cef_urlrequest_client_t> -> Ptr <cef_urlrequest_t> -> IO <void>
#callback_t cb_cef_urlrequest_client_on_upload_progress,    Ptr <cef_urlrequest_client_t> -> Ptr <cef_urlrequest_t> -> <uint64> -> <uint64> -> IO <void>
#callback_t cb_cef_urlrequest_client_on_download_progress,  Ptr <cef_urlrequest_client_t> -> Ptr <cef_urlrequest_t> -> <uint64> -> <uint64> -> IO <void>
#callback_t cb_cef_urlrequest_client_on_download_data,      Ptr <cef_urlrequest_client_t> -> Ptr <cef_urlrequest_t> -> Ptr <void> -> <size_t> -> IO <void>
#callback_t cb_cef_urlrequest_client_get_auth_credentials,  Ptr <cef_urlrequest_client_t> -> <int> -> Ptr <cef_string_t> -> <int> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> Ptr <cef_auth_callback_t> -> IO <int>


#ccall cef_urlrequest_create, Ptr <cef_request_t> -> Ptr <cef_urlrequest_client_t> -> IO (Ptr <cef_urlrequest_t>)




#ccall cef_parse_url,     Ptr <cef_string_t> -> Ptr <cef_urlparts_t> -> IO <int>
#ccall cef_create_url,    Ptr <cef_urlparts_t> -> Ptr <cef_string_t> -> IO <int>
#ccall cef_get_mime_type, Ptr <cef_string_t> -> IO <cef_string_userfree_t>






#starttype cef_v8context_t
#field base,                <cef_base_t>
#field get_task_runner,     <cb_cef_v8context_get_task_runner>
#field is_valid,            <cb_cef_v8context_is_valid>
#field get_browser,         <cb_cef_v8context_get_browser>
#field get_frame,           <cb_cef_v8context_get_frame>
#field get_global,          <cb_cef_v8context_get_global>
#field enter,               <cb_cef_v8context_enter>
#field exit,                <cb_cef_v8context_exit>
#field is_same,             <cb_cef_v8context_is_same>
#field eval,                <cb_cef_v8context_eval>
#stoptype

#callback_t cb_cef_v8context_get_task_runner,     Ptr <cef_v8context_t> -> IO (Ptr <cef_task_runner_t>)
#callback_t cb_cef_v8context_is_valid,            Ptr <cef_v8context_t> -> IO <int>
#callback_t cb_cef_v8context_get_browser,         Ptr <cef_v8context_t> -> IO (Ptr <cef_browser_t>)
#callback_t cb_cef_v8context_get_frame,           Ptr <cef_v8context_t> -> IO (Ptr <cef_frame_t>)
#callback_t cb_cef_v8context_get_global,          Ptr <cef_v8context_t> -> IO (Ptr <cef_v8value_t>)
#callback_t cb_cef_v8context_enter,               Ptr <cef_v8context_t> -> IO <int>
#callback_t cb_cef_v8context_exit,                Ptr <cef_v8context_t> -> IO <int>
#callback_t cb_cef_v8context_is_same,             Ptr <cef_v8context_t> -> Ptr <cef_v8context_t> -> IO <int>
#callback_t cb_cef_v8context_eval,                Ptr <cef_v8context_t> -> Ptr <cef_string_t> -> Ptr (Ptr <cef_v8value_t>) -> Ptr (Ptr <cef_v8exception_t>)


#starttype cef_v8handler_t
#field base,    <cef_base_t>
#field execute, <cb_cef_v8handler_execute>
#stoptype

#callback_t cb_cef_v8handler_execute, Ptr <cef_v8handler_t> -> Ptr <cef_string_t> -> Ptr <cef_v8value_t> -> <size_t> -> Ptr (Ptr <cef_v8value_t>) -> Ptr (Ptr <cef_v8value_t>) -> Ptr <cef_string_t> -> IO <int>


#starttype cef_v8accessor_t
#field base,  <cef_base_t>
#field get,   <cb_cef_v8accessor_get>
#field set,   <cb_cef_v8accessor_set>
#stoptype

#callback_t cb_cef_v8accessor_get,   Ptr <cef_v8accessor_t> -> Ptr <cef_string_t> -> Ptr <cef_v8value_t> -> Ptr (Ptr <cef_v8value_t>) -> Ptr <cef_string_t> -> IO <int>
#callback_t cb_cef_v8accessor_set,   Ptr <cef_v8accessor_t> -> Ptr <cef_string_t> -> Ptr <cef_v8value_t> -> Ptr <cef_v8value_t> -> Ptr <cef_string_t> -> IO <int>


#starttype cef_v8exception_t
#field base,                      <cef_base_t>
#field get_message,               <cb_cef_v8exception_get_message>
#field get_source_line,           <cb_cef_v8exception_get_source_line>
#field get_script_resource_name,  <cb_cef_v8exception_get_script_resource_name>
#field get_line_number,           <cb_cef_v8exception_get_line_number>
#field get_start_position,        <cb_cef_v8exception_get_start_position>
#field get_end_position,          <cb_cef_v8exception_get_end_position>
#field get_start_column,          <cb_cef_v8exception_get_start_column>
#field get_end_column,            <cb_cef_v8exception_get_end_column>
#stoptype

#callback_t cb_cef_v8exception_get_message,               Ptr <cef_v8exception_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_v8exception_get_source_line,           Ptr <cef_v8exception_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_v8exception_get_script_resource_name,  Ptr <cef_v8exception_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_v8exception_get_line_number,           Ptr <cef_v8exception_t> -> IO <int>
#callback_t cb_cef_v8exception_get_start_position,        Ptr <cef_v8exception_t> -> IO <int>
#callback_t cb_cef_v8exception_get_end_position,          Ptr <cef_v8exception_t> -> IO <int>
#callback_t cb_cef_v8exception_get_start_column,          Ptr <cef_v8exception_t> -> IO <int>
#callback_t cb_cef_v8exception_get_end_column,            Ptr <cef_v8exception_t> -> IO <int>


#starttype cef_v8value_t
#field base,                    <cef_base_t>
#field is_valid,                <cb_cef_v8value_is_valid>
#field is_undefined,            <cb_cef_v8value_is_undefined>
#field is_null,                 <cb_cef_v8value_is_null>
#field is_bool,                 <cb_cef_v8value_is_bool>
#field is_int,                  <cb_cef_v8value_is_int>
#field is_uint,                 <cb_cef_v8value_is_uint>
#field is_double,               <cb_cef_v8value_is_double>
#field is_date,                 <cb_cef_v8value_is_date>
#field is_string,               <cb_cef_v8value_is_string>
#field is_object,               <cb_cef_v8value_is_object>
#field is_array,                <cb_cef_v8value_is_array>
#field is_function,             <cb_cef_v8value_is_function>
#field is_same,                 <cb_cef_v8value_is_same>
#field get_bool_value,          <cb_cef_v8value_get_bool_value>
#field get_int_value,           <cb_cef_v8value_get_int_value>
#field get_uint_value,          <cb_cef_v8value_get_uint_value>
#field get_double_value,        <cb_cef_v8value_get_double_value>
#field get_date_value,          FunPtr (Ptr <cef_v8value_t> -> IO <cef_time_t>)
#field get_string_value,        <cb_cef_v8value_get_string_value>
#field is_user_created,         <cb_cef_v8value_is_user_created>
#field has_exception,           <cb_cef_v8value_has_exception>
#field get_exception,           <cb_cef_v8value_get_exception>
#field clear_exception,         <cb_cef_v8value_clear_exception>
#field will_rethrow_exceptions, <cb_cef_v8value_will_rethrow_exceptions>
#field set_rethrow_exceptions,  <cb_cef_v8value_set_rethrow_exceptions>
#field has_value_bykey,         <cb_cef_v8value_has_value_bykey>
#field has_value_byindex,       <cb_cef_v8value_has_value_byindex>
#field delete_value_bykey,      <cb_cef_v8value_delete_value_bykey>
#field delete_value_byindex,    <cb_cef_v8value_delete_value_byindex>
#field get_value_bykey,         <cb_cef_v8value_get_value_bykey>
#field get_value_byindex,       <cb_cef_v8value_get_value_byindex>
#field set_value_bykey,         <cb_cef_v8value_set_value_bykey>
#field set_value_byindex,       <cb_cef_v8value_set_value_byindex>
#field set_value_byaccessor,    <cb_cef_v8value_set_value_byaccessor>
#field get_keys,                <cb_cef_v8value_get_keys>
#field set_user_data,           <cb_cef_v8value_set_user_data>
#field get_user_data,           <cb_cef_v8value_get_user_data>
#field get_externally_allocated_memory,     <cb_cef_v8value_get_externally_allocated_memory>
#field adjust_externally_allocated_memory,  <cb_cef_v8value_adjust_externally_allocated_memory>
#field get_array_length,        <cb_cef_v8value_get_array_length>
#field get_function_name,       <cb_cef_v8value_get_function_name>
#field get_function_handler,    <cb_cef_v8value_get_function_handler>
#field execute_function,        <cb_cef_v8value_execute_function>
#field execute_function_with_context,       <cb_cef_v8value_execute_function_with_context>
#stoptype

#callback_t cb_cef_v8value_is_valid,                Ptr <cef_v8value_t> -> IO <int>
#callback_t cb_cef_v8value_is_undefined,            Ptr <cef_v8value_t> -> IO <int>
#callback_t cb_cef_v8value_is_null,                 Ptr <cef_v8value_t> -> IO <int>
#callback_t cb_cef_v8value_is_bool,                 Ptr <cef_v8value_t> -> IO <int>
#callback_t cb_cef_v8value_is_int,                  Ptr <cef_v8value_t> -> IO <int>
#callback_t cb_cef_v8value_is_uint,                 Ptr <cef_v8value_t> -> IO <int>
#callback_t cb_cef_v8value_is_double,               Ptr <cef_v8value_t> -> IO <int>
#callback_t cb_cef_v8value_is_date,                 Ptr <cef_v8value_t> -> IO <int>
#callback_t cb_cef_v8value_is_string,               Ptr <cef_v8value_t> -> IO <int>
#callback_t cb_cef_v8value_is_object,               Ptr <cef_v8value_t> -> IO <int>
#callback_t cb_cef_v8value_is_array,                Ptr <cef_v8value_t> -> IO <int>
#callback_t cb_cef_v8value_is_function,             Ptr <cef_v8value_t> -> IO <int>
#callback_t cb_cef_v8value_is_same,                 Ptr <cef_v8value_t> -> Ptr <cef_v8value_t> -> IO <int>
#callback_t cb_cef_v8value_get_bool_value,          Ptr <cef_v8value_t> -> IO <int>
#callback_t cb_cef_v8value_get_int_value,           Ptr <cef_v8value_t> -> IO <int32>
#callback_t cb_cef_v8value_get_uint_value,          Ptr <cef_v8value_t> -> IO <uint32>
#callback_t cb_cef_v8value_get_double_value,        Ptr <cef_v8value_t> -> IO <double>
#callback_t cb_cef_v8value_get_string_value,        Ptr <cef_v8value_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_v8value_is_user_created,         Ptr <cef_v8value_t> -> IO <int>
#callback_t cb_cef_v8value_has_exception,           Ptr <cef_v8value_t> -> IO <int>
#callback_t cb_cef_v8value_get_exception,           Ptr <cef_v8value_t> -> IO (Ptr <cef_v8exception_t>)
#callback_t cb_cef_v8value_clear_exception,         Ptr <cef_v8value_t> -> IO <int>
#callback_t cb_cef_v8value_will_rethrow_exceptions, Ptr <cef_v8value_t> -> IO <int>
#callback_t cb_cef_v8value_set_rethrow_exceptions,  Ptr <cef_v8value_t> -> <int> -> IO <int>
#callback_t cb_cef_v8value_has_value_bykey,         Ptr <cef_v8value_t> -> Ptr <cef_string_t> -> IO <int>
#callback_t cb_cef_v8value_has_value_byindex,       Ptr <cef_v8value_t> -> <int> -> IO <int>
#callback_t cb_cef_v8value_delete_value_bykey,      Ptr <cef_v8value_t> -> Ptr <cef_string_t> -> IO <int>
#callback_t cb_cef_v8value_delete_value_byindex,    Ptr <cef_v8value_t> -> <int> -> IO <int>
#callback_t cb_cef_v8value_get_value_bykey,         Ptr <cef_v8value_t> -> Ptr <cef_string_t> -> IO (Ptr <cef_v8value_t>)
#callback_t cb_cef_v8value_get_value_byindex,       Ptr <cef_v8value_t> -> <int> -> IO (Ptr <cef_v8value_t>)
#callback_t cb_cef_v8value_set_value_bykey,         Ptr <cef_v8value_t> -> Ptr <cef_string_t> -> Ptr <cef_v8value_t> -> <cef_v8_propertyattribute_t> -> IO <int>
#callback_t cb_cef_v8value_set_value_byindex,       Ptr <cef_v8value_t> -> <int> -> Ptr <cef_v8value_t> -> IO <int>
#callback_t cb_cef_v8value_set_value_byaccessor,    Ptr <cef_v8value_t> -> Ptr <cef_string_t> -> <cef_v8_accesscontrol_t> -> <cef_v8_propertyattribute_t> -> IO <int>
#callback_t cb_cef_v8value_get_keys,                Ptr <cef_v8value_t> -> <cef_string_list_t> -> IO <int>
#callback_t cb_cef_v8value_set_user_data,           Ptr <cef_v8value_t> -> Ptr <cef_base_t> -> IO <int>
#callback_t cb_cef_v8value_get_user_data,           Ptr <cef_v8value_t> -> IO (Ptr <cef_base_t>)
#callback_t cb_cef_v8value_get_externally_allocated_memory,     Ptr <cef_v8value_t> -> IO <int>
#callback_t cb_cef_v8value_adjust_externally_allocated_memory,  Ptr <cef_v8value_t> -> <int> -> IO <int>
#callback_t cb_cef_v8value_get_array_length,        Ptr <cef_v8value_t> -> IO <int>
#callback_t cb_cef_v8value_get_function_name,       Ptr <cef_v8value_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_v8value_get_function_handler,    Ptr <cef_v8value_t> -> IO (Ptr <cef_v8handler_t>)
#callback_t cb_cef_v8value_execute_function,        Ptr <cef_v8value_t> -> Ptr <cef_v8value_t> -> <size_t> -> Ptr (Ptr <cef_v8value_t>) -> IO (Ptr <cef_v8value_t>)
#callback_t cb_cef_v8value_execute_function_with_context,       Ptr <cef_v8value_t> -> Ptr <cef_v8context_t> -> Ptr <cef_v8value_t> -> <size_t> -> Ptr <cef_v8value_t> -> IO (Ptr <cef_v8value_t>)


#starttype cef_v8stack_trace_t
#field base,            <cef_base_t>
#field is_valid,        <cb_cef_v8stack_trace_is_valid>
#field get_frame_count, <cb_cef_v8stack_trace_get_frame_count>
#field get_frame,       <cb_cef_v8stack_trace_get_frame>
#stoptype

#callback_t cb_cef_v8stack_trace_is_valid,        Ptr <cef_v8stack_trace_t> -> IO <int>
#callback_t cb_cef_v8stack_trace_get_frame_count, Ptr <cef_v8stack_trace_t> -> IO <int>
#callback_t cb_cef_v8stack_trace_get_frame,       Ptr <cef_v8stack_trace_t> -> <int> -> IO (Ptr <cef_v8stack_frame_t>)


#starttype cef_v8stack_frame_t
#field base,              <cef_base_t>
#field is_valid,          <cb_cef_v8stack_frame_is_valid>
#field get_script_name,   <cb_cef_v8stack_frame_get_script_name>
#field get_script_name_or_source_url, <cb_cef_v8stack_frame_get_script_name_or_source_url>
#field get_function_name, <cb_cef_v8stack_frame_get_function_name>
#field get_line_number,   <cb_cef_v8stack_frame_get_line_number>
#field get_column,        <cb_cef_v8stack_frame_get_column>
#field is_eval,           <cb_cef_v8stack_frame_is_eval>
#field is_constructor,    <cb_cef_v8stack_frame_is_constructor>
#stoptype

#callback_t cb_cef_v8stack_frame_is_valid,          Ptr <cef_v8stack_frame_t> -> IO <int>
#callback_t cb_cef_v8stack_frame_get_script_name,   Ptr <cef_v8stack_frame_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_v8stack_frame_get_script_name_or_source_url, Ptr <cef_v8stack_frame_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_v8stack_frame_get_function_name, Ptr <cef_v8stack_frame_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_v8stack_frame_get_line_number,   Ptr <cef_v8stack_frame_t> -> IO <int>
#callback_t cb_cef_v8stack_frame_get_column,        Ptr <cef_v8stack_frame_t> -> IO <int>
#callback_t cb_cef_v8stack_frame_is_eval,           Ptr <cef_v8stack_frame_t> -> IO <int>
#callback_t cb_cef_v8stack_frame_is_constructor,    Ptr <cef_v8stack_frame_t> -> IO <int>


#ccall cef_v8context_get_current_context, IO (Ptr <cef_v8context_t>)
#ccall cef_v8context_get_entered_context, IO (Ptr <cef_v8context_t>)
#ccall cef_v8context_in_context,          IO <int>

#ccall cef_v8value_create_undefined,  IO (Ptr <cef_v8value_t>)
#ccall cef_v8value_create_null,       IO (Ptr <cef_v8value_t>)
#ccall cef_v8value_create_bool,       <int> -> IO (Ptr <cef_v8value_t>)
#ccall cef_v8value_create_int,        <int32> -> IO (Ptr <cef_v8value_t>)
#ccall cef_v8value_create_uint,       <uint32> -> IO (Ptr <cef_v8value_t>)
#ccall cef_v8value_create_double,     <double> -> IO (Ptr <cef_v8value_t>)
#ccall cef_v8value_create_date,       Ptr <cef_time_t> -> IO (Ptr <cef_v8value_t>)
#ccall cef_v8value_create_string,     Ptr <cef_time_t> -> IO (Ptr <cef_v8value_t>)
#ccall cef_v8value_create_object,     Ptr <cef_v8accessor_t> -> IO (Ptr <cef_v8value_t>)
#ccall cef_v8value_create_array,      <int> -> IO (Ptr <cef_v8value_t>)
#ccall cef_v8value_create_function,   Ptr <cef_string_t> -> Ptr <cef_v8handler_t> -> IO (Ptr <cef_v8value_t>)

#ccall cef_v8stack_trace_get_current,   <int> -> IO (Ptr <cef_v8stack_trace_t>)

#ccall cef_register_extension,  Ptr <cef_string_t> -> Ptr <cef_string_t> -> Ptr <cef_v8handler_t> -> IO <int>








#starttype cef_binary_value_t
#field base,      <cef_base_t>
#field is_valid,  <cb_cef_binary_value_is_valid>
#field is_owned,  <cb_cef_binary_value_is_owned>
#field copy,      <cb_cef_binary_value_copy>
#field get_size,  <cb_cef_binary_value_get_size>
#field get_data,  <cb_cef_binary_value_get_data>
#stoptype

#callback_t cb_cef_binary_value_is_valid,  Ptr <cef_binary_value_t> -> IO <int>
#callback_t cb_cef_binary_value_is_owned,  Ptr <cef_binary_value_t> -> IO <int>
#callback_t cb_cef_binary_value_copy,      Ptr <cef_binary_value_t> -> IO (Ptr <cef_binary_value_t>)
#callback_t cb_cef_binary_value_get_size,  Ptr <cef_binary_value_t> -> IO <size_t>
#callback_t cb_cef_binary_value_get_data,  Ptr <cef_binary_value_t> -> Ptr <void> -> <size_t> -> <size_t> -> IO <size_t>


#starttype cef_dictionary_value_t
#field base,            <cef_base_t>
#field is_valid,        <cb_cef_dictionary_value_is_valid>
#field is_owned,        <cb_cef_dictionary_value_is_owned>
#field is_read_only,    <cb_cef_dictionary_value_is_read_only>
#field copy,            <cb_cef_dictionary_value_copy>
#field get_size,        <cb_cef_dictionary_value_get_size>
#field clear,           <cb_cef_dictionary_value_clear>
#field has_key,         <cb_cef_dictionary_value_has_key>
#field get_keys,        <cb_cef_dictionary_value_get_keys>
#field remove,          <cb_cef_dictionary_value_remove>
#field get_type,        <cb_cef_dictionary_value_get_type>
#field get_bool,        <cb_cef_dictionary_value_get_bool>
#field get_int,         <cb_cef_dictionary_value_get_int>
#field get_double,      <cb_cef_dictionary_value_get_double>
#field get_string,      <cb_cef_dictionary_value_get_string>
#field get_binary,      <cb_cef_dictionary_value_get_binary>
#field get_dictionary,  <cb_cef_dictionary_value_get_dictionary>
#field get_list,        <cb_cef_dictionary_value_get_list>
#field set_null,        <cb_cef_dictionary_value_set_null>
#field set_bool,        <cb_cef_dictionary_value_set_bool>
#field set_int,         <cb_cef_dictionary_value_set_int>
#field set_double,      <cb_cef_dictionary_value_set_double>
#field set_string,      <cb_cef_dictionary_value_set_string>
#field set_binary,      <cb_cef_dictionary_value_set_binary>
#field set_dictionary,  <cb_cef_dictionary_value_set_dictionary>
#field set_list,        <cb_cef_dictionary_value_set_list>
#stoptype

#callback_t cb_cef_dictionary_value_is_valid,        Ptr <cef_dictionary_value_t> -> IO <int>
#callback_t cb_cef_dictionary_value_is_owned,        Ptr <cef_dictionary_value_t> -> IO <int>
#callback_t cb_cef_dictionary_value_is_read_only,    Ptr <cef_dictionary_value_t> -> IO <int>
#callback_t cb_cef_dictionary_value_copy,            Ptr <cef_dictionary_value_t> -> <int> -> IO (Ptr <cef_dictionary_value_t>)
#callback_t cb_cef_dictionary_value_get_size,        Ptr <cef_dictionary_value_t> -> IO <size_t>
#callback_t cb_cef_dictionary_value_clear,           Ptr <cef_dictionary_value_t> -> IO <int>
#callback_t cb_cef_dictionary_value_has_key,         Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> IO <int>
#callback_t cb_cef_dictionary_value_get_keys,        Ptr <cef_dictionary_value_t> -> <cef_string_list_t> -> IO <int>
#callback_t cb_cef_dictionary_value_remove,          Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> IO <int>
#callback_t cb_cef_dictionary_value_get_type,        Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> IO <cef_value_type_t>
#callback_t cb_cef_dictionary_value_get_bool,        Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> IO <int>
#callback_t cb_cef_dictionary_value_get_int,         Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> IO <int>
#callback_t cb_cef_dictionary_value_get_double,      Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> IO <double>
#callback_t cb_cef_dictionary_value_get_string,      Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_dictionary_value_get_binary,      Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> IO (Ptr <cef_binary_value_t>)
#callback_t cb_cef_dictionary_value_get_dictionary,  Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> IO (Ptr <cef_dictionary_value_t>)
#callback_t cb_cef_dictionary_value_get_list,        Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> IO (Ptr <cef_list_value_t>)
#callback_t cb_cef_dictionary_value_set_null,        Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> IO <int>
#callback_t cb_cef_dictionary_value_set_bool,        Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> <int> -> IO <int>
#callback_t cb_cef_dictionary_value_set_int,         Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> <int> -> IO <int>
#callback_t cb_cef_dictionary_value_set_double,      Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> <double> -> IO <int>
#callback_t cb_cef_dictionary_value_set_string,      Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> IO <int>
#callback_t cb_cef_dictionary_value_set_binary,      Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> Ptr <cef_binary_value_t> -> IO <int>
#callback_t cb_cef_dictionary_value_set_dictionary,  Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> Ptr <cef_dictionary_value_t> -> IO <int>
#callback_t cb_cef_dictionary_value_set_list,        Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> Ptr <cef_list_value_t> -> IO <int>


#starttype cef_list_value_t
#field base,            <cef_base_t>
#field is_valid,        <cb_cef_list_value_is_valid>
#field is_owned,        <cb_cef_list_value_is_owned>
#field is_read_only,    <cb_cef_list_value_is_read_only>
#field copy,            <cb_cef_list_value_copy>
#field set_size,        <cb_cef_list_value_set_size>
#field get_size,        <cb_cef_list_value_get_size>
#field clear,           <cb_cef_list_value_clear>
#field remove,          <cb_cef_list_value_remove>
#field get_type,        <cb_cef_list_value_get_type>
#field get_bool,        <cb_cef_list_value_get_bool>
#field get_int,         <cb_cef_list_value_get_int>
#field get_double,      <cb_cef_list_value_get_double>
#field get_string,      <cb_cef_list_value_get_string>
#field get_binary,      <cb_cef_list_value_get_binary>
#field get_dictionary,  <cb_cef_list_value_get_dictionary>
#field get_list,        <cb_cef_list_value_get_list>
#field set_null,        <cb_cef_list_value_set_null>
#field set_bool,        <cb_cef_list_value_set_bool>
#field set_int,         <cb_cef_list_value_set_int>
#field set_double,      <cb_cef_list_value_set_double>
#field set_string,      <cb_cef_list_value_set_string>
#field set_binary,      <cb_cef_list_value_set_binary>
#field set_dictionary,  <cb_cef_list_value_set_dictionary>
#field set_list,        <cb_cef_list_value_set_list>
#stoptype

#callback_t cb_cef_list_value_is_valid,        Ptr <cef_list_value_t> -> IO <int>
#callback_t cb_cef_list_value_is_owned,        Ptr <cef_list_value_t> -> IO <int>
#callback_t cb_cef_list_value_is_read_only,    Ptr <cef_list_value_t> -> IO <int>
#callback_t cb_cef_list_value_copy,            Ptr <cef_list_value_t> -> IO (Ptr <cef_list_value_t>)
#callback_t cb_cef_list_value_set_size,        Ptr <cef_list_value_t> -> <size_t> -> IO <int>
#callback_t cb_cef_list_value_get_size,        Ptr <cef_list_value_t> -> IO <size_t>
#callback_t cb_cef_list_value_clear,           Ptr <cef_list_value_t> -> IO <int>
#callback_t cb_cef_list_value_remove,          Ptr <cef_list_value_t> -> <int> -> IO <int>
#callback_t cb_cef_list_value_get_type,        Ptr <cef_list_value_t> -> <int> -> IO <cef_value_type_t>
#callback_t cb_cef_list_value_get_bool,        Ptr <cef_list_value_t> -> <int> -> IO <int>
#callback_t cb_cef_list_value_get_int,         Ptr <cef_list_value_t> -> <int> -> IO <int>
#callback_t cb_cef_list_value_get_double,      Ptr <cef_list_value_t> -> <int> -> IO <double>
#callback_t cb_cef_list_value_get_string,      Ptr <cef_list_value_t> -> <int> -> IO <cef_string_userfree_t>
#callback_t cb_cef_list_value_get_binary,      Ptr <cef_list_value_t> -> <int> -> IO (Ptr <cef_binary_value_t>)
#callback_t cb_cef_list_value_get_dictionary,  Ptr <cef_list_value_t> -> <int> -> IO (Ptr <cef_dictionary_value_t>)
#callback_t cb_cef_list_value_get_list,        Ptr <cef_list_value_t> -> <int> -> IO (Ptr <cef_list_value_t>)
#callback_t cb_cef_list_value_set_null,        Ptr <cef_list_value_t> -> <int> -> IO <int>
#callback_t cb_cef_list_value_set_bool,        Ptr <cef_list_value_t> -> <int> -> <int> -> IO <int>
#callback_t cb_cef_list_value_set_int,         Ptr <cef_list_value_t> -> <int> -> <int> -> IO <int>
#callback_t cb_cef_list_value_set_double,      Ptr <cef_list_value_t> -> <int> -> <double> -> IO <int>
#callback_t cb_cef_list_value_set_string,      Ptr <cef_list_value_t> -> <int> -> Ptr <cef_string_t> -> IO <int>
#callback_t cb_cef_list_value_set_binary,      Ptr <cef_list_value_t> -> <int> -> Ptr <cef_binary_value_t> -> IO <int>
#callback_t cb_cef_list_value_set_dictionary,  Ptr <cef_list_value_t> -> <int> -> Ptr <cef_dictionary_value_t> -> IO <int>
#callback_t cb_cef_list_value_set_list,        Ptr <cef_list_value_t> -> <int> -> Ptr <cef_list_value_t> -> IO <int>


#ccall cef_binary_value_create, Ptr <void> -> <size_t> -> IO (Ptr <cef_binary_value_t>)
#ccall cef_dictionary_value_create, IO (Ptr <cef_dictionary_value_t>)
#ccall cef_list_value_create, IO (Ptr <cef_list_value_t>)







#starttype cef_web_plugin_info_t
#field base,            <cef_base_t>
#field get_name,        <cb_cef_web_plugin_info_get_name>
#field get_path,        <cb_cef_web_plugin_info_get_path>
#field get_version,     <cb_cef_web_plugin_info_get_version>
#field get_description, <cb_cef_web_plugin_info_get_description>
#stoptype

#callback_t cb_cef_web_plugin_info_get_name,        Ptr <cef_web_plugin_info_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_web_plugin_info_get_path,        Ptr <cef_web_plugin_info_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_web_plugin_info_get_version,     Ptr <cef_web_plugin_info_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_web_plugin_info_get_description, Ptr <cef_web_plugin_info_t> -> IO <cef_string_userfree_t>


#starttype cef_web_plugin_info_visitor_t
#field base,  <cef_base_t>
#field visit, <cb_cef_web_plugin_info_visitor_visit>
#stoptype

#callback_t cb_cef_web_plugin_info_visitor_visit, Ptr <cef_web_plugin_info_visitor_t> -> Ptr <cef_web_plugin_info_t> -> <int> -> <int> -> IO <int>


#starttype cef_web_plugin_unstable_callback_t
#field base,        <cef_base_t>
#field is_unstable, <cb_cef_web_plugin_unstable_callback_is_unstable>
#stoptype

#callback_t cb_cef_web_plugin_unstable_callback_is_unstable, Ptr <cef_web_plugin_unstable_callback_t> -> Ptr <cef_string_t> -> <int> -> IO <void>


#ccall cef_visit_web_plugin_info,           Ptr <cef_web_plugin_info_visitor_t> -> IO <void>
#ccall cef_refresh_web_plugins,             IO <void>
#ccall cef_add_web_plugin_path,             Ptr <cef_string_t> -> IO <void>
#ccall cef_add_web_plugin_directory,        Ptr <cef_string_t> -> IO <void>
#ccall cef_remove_web_plugin_path,          Ptr <cef_string_t> -> IO <void>
#ccall cef_unregister_internal_web_plugin,  Ptr <cef_string_t> -> IO <void>
#ccall cef_force_web_plugin_shutdown,       Ptr <cef_string_t> -> IO <void>
#ccall cef_register_web_plugin_crash,       Ptr <cef_string_t> -> IO <void>
#ccall cef_is_web_plugin_unstable,          Ptr <cef_string_t> -> Ptr <cef_web_plugin_unstable_callback_t> -> IO <void>






#starttype cef_xml_reader_t
#field base,                      <cef_base_t>
#field move_to_next_node,         <cb_cef_xml_reader_move_to_next_node>
#field close,                     <cb_cef_xml_reader_close>
#field has_error,                 <cb_cef_xml_reader_has_error>
#field get_error,                 <cb_cef_xml_reader_get_error>
#field get_type,                  <cb_cef_xml_reader_get_type>
#field get_depth,                 <cb_cef_xml_reader_get_depth>
#field get_local_name,            <cb_cef_xml_reader_get_local_name>
#field get_prefix,                <cb_cef_xml_reader_get_prefix>
#field get_qualified_name,        <cb_cef_xml_reader_get_qualified_name>
#field get_namespace_uri,         <cb_cef_xml_reader_get_namespace_uri>
#field get_base_uri,              <cb_cef_xml_reader_get_base_uri>
#field get_xml_lang,              <cb_cef_xml_reader_get_xml_lang>
#field is_empty_element,          <cb_cef_xml_reader_is_empty_element>
#field has_value,                 <cb_cef_xml_reader_has_value>
#field get_value,                 <cb_cef_xml_reader_get_value>
#field has_attributes,            <cb_cef_xml_reader_has_attributes>
#field get_attribute_count,       <cb_cef_xml_reader_get_attribute_count>
#field get_attribute_byindex,     <cb_cef_xml_reader_get_attribute_byindex>
#field get_attribute_byqname,     <cb_cef_xml_reader_get_attribute_byqname>
#field get_attribute_bylname,     <cb_cef_xml_reader_get_attribute_bylname>
#field get_inner_xml,             <cb_cef_xml_reader_get_inner_xml>
#field get_outer_xml,             <cb_cef_xml_reader_get_outer_xml>
#field get_line_number,           <cb_cef_xml_reader_get_line_number>
#field move_to_attribute_byindex, <cb_cef_xml_reader_move_to_attribute_byindex>
#field move_to_attribute_byqname, <cb_cef_xml_reader_move_to_attribute_byqname>
#field move_to_attribute_bylname, <cb_cef_xml_reader_move_to_attribute_bylname>
#field move_to_first_attribute,   <cb_cef_xml_reader_move_to_first_attribute>
#field move_to_next_attribute,    <cb_cef_xml_reader_move_to_next_attribute>
#field move_to_carrying_element,  <cb_cef_xml_reader_move_to_carrying_element>
#stoptype

#callback_t cb_cef_xml_reader_move_to_next_node,         Ptr <cef_xml_reader_t> -> IO <int>
#callback_t cb_cef_xml_reader_close,                     Ptr <cef_xml_reader_t> -> IO <int>
#callback_t cb_cef_xml_reader_has_error,                 Ptr <cef_xml_reader_t> -> IO <int>
#callback_t cb_cef_xml_reader_get_error,                 Ptr <cef_xml_reader_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_xml_reader_get_type,                  Ptr <cef_xml_reader_t> -> IO <cef_xml_node_type_t>
#callback_t cb_cef_xml_reader_get_depth,                 Ptr <cef_xml_reader_t> -> IO <int>
#callback_t cb_cef_xml_reader_get_local_name,            Ptr <cef_xml_reader_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_xml_reader_get_prefix,                Ptr <cef_xml_reader_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_xml_reader_get_qualified_name,        Ptr <cef_xml_reader_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_xml_reader_get_namespace_uri,         Ptr <cef_xml_reader_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_xml_reader_get_base_uri,              Ptr <cef_xml_reader_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_xml_reader_get_xml_lang,              Ptr <cef_xml_reader_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_xml_reader_is_empty_element,          Ptr <cef_xml_reader_t> -> IO <int>
#callback_t cb_cef_xml_reader_has_value,                 Ptr <cef_xml_reader_t> -> IO <int>
#callback_t cb_cef_xml_reader_get_value,                 Ptr <cef_xml_reader_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_xml_reader_has_attributes,            Ptr <cef_xml_reader_t> -> IO <int>
#callback_t cb_cef_xml_reader_get_attribute_count,       Ptr <cef_xml_reader_t> -> IO <size_t>
#callback_t cb_cef_xml_reader_get_attribute_byindex,     Ptr <cef_xml_reader_t> -> <int> -> IO <cef_string_userfree_t>
#callback_t cb_cef_xml_reader_get_attribute_byqname,     Ptr <cef_xml_reader_t> -> Ptr <cef_string_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_xml_reader_get_attribute_bylname,     Ptr <cef_xml_reader_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_xml_reader_get_inner_xml,             Ptr <cef_xml_reader_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_xml_reader_get_outer_xml,             Ptr <cef_xml_reader_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_xml_reader_get_line_number,           Ptr <cef_xml_reader_t> -> IO <int>
#callback_t cb_cef_xml_reader_move_to_attribute_byindex, Ptr <cef_xml_reader_t> -> <int> -> IO <int>
#callback_t cb_cef_xml_reader_move_to_attribute_byqname, Ptr <cef_xml_reader_t> -> Ptr <cef_string_t> -> IO <int>
#callback_t cb_cef_xml_reader_move_to_attribute_bylname, Ptr <cef_xml_reader_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> IO <int>
#callback_t cb_cef_xml_reader_move_to_first_attribute,   Ptr <cef_xml_reader_t> -> IO <int>
#callback_t cb_cef_xml_reader_move_to_next_attribute,    Ptr <cef_xml_reader_t> -> IO <int>
#callback_t cb_cef_xml_reader_move_to_carrying_element,  Ptr <cef_xml_reader_t> -> IO <int>


#ccall cef_xml_reader_create,     Ptr <cef_stream_reader_t> -> <cef_xml_encoding_type_t> -> Ptr <cef_string_t> -> IO (Ptr <cef_xml_reader_t>)





#starttype cef_zip_reader_t
#field base,                    <cef_base_t>
#field move_to_first_file,      <cb_cef_zip_reader_move_to_first_file>
#field move_to_next_file,       <cb_cef_zip_reader_move_to_next_file>
#field move_to_file,            <cb_cef_zip_reader_move_to_file>
#field close,                   <cb_cef_zip_reader_close>
#field get_file_name,           <cb_cef_zip_reader_get_file_name>
#field get_file_size,           <cb_cef_zip_reader_get_file_size>
#field get_file_last_modified,  <cb_cef_zip_reader_get_file_last_modified>
#field open_file,               <cb_cef_zip_reader_open_file>
#field close_file,              <cb_cef_zip_reader_close_file>
#field read_file,               <cb_cef_zip_reader_read_file>
#field tell,                    <cb_cef_zip_reader_tell>
#field eof,                     <cb_cef_zip_reader_eof>
#stoptype

#callback_t cb_cef_zip_reader_move_to_first_file,      Ptr <cef_zip_reader_t> -> IO <int>
#callback_t cb_cef_zip_reader_move_to_next_file,       Ptr <cef_zip_reader_t> -> IO <int>
#callback_t cb_cef_zip_reader_move_to_file,            Ptr <cef_zip_reader_t> -> Ptr <cef_string_t> -> <int> -> IO <int>
#callback_t cb_cef_zip_reader_close,                   Ptr <cef_zip_reader_t> -> IO <int>
#callback_t cb_cef_zip_reader_get_file_name,           Ptr <cef_zip_reader_t> -> IO <cef_string_userfree_t>
#callback_t cb_cef_zip_reader_get_file_size,           Ptr <cef_zip_reader_t> -> IO <int64>
#callback_t cb_cef_zip_reader_get_file_last_modified,  Ptr <cef_zip_reader_t> -> IO <time_t>
#callback_t cb_cef_zip_reader_open_file,               Ptr <cef_zip_reader_t> -> Ptr <cef_string_t> -> IO <int>
#callback_t cb_cef_zip_reader_close_file,              Ptr <cef_zip_reader_t> -> IO <int>
#callback_t cb_cef_zip_reader_read_file,               Ptr <cef_zip_reader_t> -> Ptr <void> -> <size_t> -> IO <int>
#callback_t cb_cef_zip_reader_tell,                    Ptr <cef_zip_reader_t> -> IO <int64>
#callback_t cb_cef_zip_reader_eof,                     Ptr <cef_zip_reader_t> -> IO <int>


#ccall cef_zip_reader_create,   Ptr <cef_stream_reader_t> -> IO (Ptr <cef_zip_reader_t>)
