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


#starttype cef_base_t
#field size,      <size_t>
#field add_ref,   FunPtr (Ptr <cef_base_t> -> IO <int>)
#field release,   FunPtr (Ptr <cef_base_t> -> IO <int>)
#field get_refct, FunPtr (Ptr <cef_base_t> -> IO <int>)
#stoptype



#starttype cef_app_t
#field base,                              <cef_base_t>
#field on_before_command_line_processing, FunPtr (Ptr <cef_app_t> -> Ptr <cef_string_t> -> Ptr <cef_command_line_t> -> IO <void>)
#field on_register_custom_schemes,        FunPtr (Ptr <cef_app_t> -> Ptr <cef_scheme_registrar_t> -> IO <void>)
#field get_resource_bundle_handler,       FunPtr (Ptr <cef_app_t> -> IO (Ptr <cef_resource_bundle_handler_t>))
#field get_browser_process_handler,       FunPtr (Ptr <cef_app_t> -> IO (Ptr <cef_browser_process_handler_t>))
#field get_render_process_handler,        FunPtr (Ptr <cef_app_t> -> IO (Ptr <cef_render_process_handler_t>))
#stoptype

#ccall  cef_execute_process,        Ptr <cef_main_args_t> -> Ptr <cef_app_t> -> Ptr <void> -> IO <int>
#ccall  cef_initialize,             Ptr <cef_main_args_t> -> Ptr <cef_settings_t> -> Ptr <cef_app_t> -> Ptr <void> -> IO <int>
#ccall  cef_shutdown,               IO <void>
#ccall  cef_do_message_loop_work,   IO <void>
#ccall  cef_run_message_loop,       IO <void>
#ccall  cef_quit_message_loop,      IO <void>
#ccall  cef_set_osmodal_loop,       <int> -> IO <void>




#starttype cef_auth_callback_t
#field base,    <cef_base_t>
#field cont,    FunPtr (Ptr <cef_auth_callback_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> IO <void>)
#field cancel,  FunPtr (Ptr <cef_auth_callback_t> -> IO <void>)
#stoptype





#starttype cef_browser_t
#field base,                <cef_base_t>
#field get_host,            FunPtr (Ptr <cef_browser_t> -> IO (Ptr <cef_browser_host_t>))
#field can_go_back,         FunPtr (Ptr <cef_browser_t> -> IO <int>)
#field go_back,             FunPtr (Ptr <cef_browser_t> -> IO <int>)
#field can_go_forward,      FunPtr (Ptr <cef_browser_t> -> IO <int>)
#field go_forward,          FunPtr (Ptr <cef_browser_t> -> IO <int>)
#field is_loading,          FunPtr (Ptr <cef_browser_t> -> IO <int>)
#field reload,              FunPtr (Ptr <cef_browser_t> -> IO <void>)
#field reload_ignore_cache, FunPtr (Ptr <cef_browser_t> -> IO <void>)
#field stop_load,           FunPtr (Ptr <cef_browser_t> -> IO <void>)
#field get_identifier,      FunPtr (Ptr <cef_browser_t> -> IO <int>)
#field is_same,             FunPtr (Ptr <cef_browser_t> -> Ptr <cef_browser_t> -> IO <int>)
#field is_popup,            FunPtr (Ptr <cef_browser_t> -> IO <int>)
#field has_document,        FunPtr (Ptr <cef_browser_t> -> IO <int>)
#field get_main_frame,      FunPtr (Ptr <cef_browser_t> -> IO (Ptr <cef_frame_t>))
#field get_focused_frame,   FunPtr (Ptr <cef_browser_t> -> IO (Ptr <cef_frame_t>))
#field get_frame_byident,   FunPtr (Ptr <cef_browser_t> -> <int64> -> IO (Ptr <cef_frame_t>))
#field get_frame,           FunPtr (Ptr <cef_browser_t> -> Ptr <cef_string_t> -> IO (Ptr <cef_frame_t>))
#field get_frame_count,     FunPtr (Ptr <cef_browser_t> -> IO <size_t>)
#field get_frame_identifiers, FunPtr (Ptr <cef_browser_t> -> Ptr <size_t> -> Ptr <int64> -> IO <void>)
#field get_frame_names,     FunPtr (Ptr <cef_browser_t> -> <cef_string_list_t> -> IO <void>)
#field send_process_message, FunPtr (Ptr <cef_browser_t> -> <cef_process_id_t> -> Ptr <cef_process_message_t> -> IO <int>)
#stoptype

#starttype cef_run_file_dialog_callback_t
#field base, <cef_base_t>
#field cont, FunPtr (Ptr <cef_browser_t> -> Ptr <cef_browser_host_t> -> <cef_string_list_t> -> IO <void>)
#stoptype

#starttype cef_browser_host_t
#field base,                  <cef_base_t>
#field get_browser,           FunPtr (Ptr <cef_browser_host_t> -> IO (Ptr <cef_browser_t>))
#field parent_window_will_close, FunPtr (Ptr <cef_browser_host_t> -> IO <void>)
#field close_browser,         FunPtr (Ptr <cef_browser_host_t> -> <int> -> IO <void>)
#field set_focus,             FunPtr (Ptr <cef_browser_host_t> -> <int> -> IO <void>)
#field set_window_visibility, FunPtr (Ptr <cef_browser_host_t> -> <int> -> IO <void>)
#field get_window_handle,     FunPtr (Ptr <cef_browser_host_t> -> IO <cef_window_handle_t>)
#field get_opener_window_handle, FunPtr (Ptr <cef_browser_host_t> -> IO <cef_window_handle_t>)
#field get_client,            FunPtr (Ptr <cef_browser_host_t> -> IO (Ptr <cef_client_t>))
#field get_request_context,   FunPtr (Ptr <cef_browser_host_t> -> IO (Ptr <cef_request_context_t>))
#field get_zoom_level,        FunPtr (Ptr <cef_browser_host_t> -> IO <double>)
#field set_zoom_level,        FunPtr (Ptr <cef_browser_host_t> -> <double> -> IO <void>)
#field run_file_dialog,       FunPtr (Ptr <cef_browser_host_t> -> <cef_file_dialog_mode_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> <cef_string_list_t> -> Ptr <cef_run_file_dialog_callback_t> -> IO <void>)
#field start_download,        FunPtr (Ptr <cef_browser_host_t> -> Ptr <cef_string_t> -> IO <void>)
#field print,                 FunPtr (Ptr <cef_browser_host_t> -> IO <void>)
#field find,                  FunPtr (Ptr <cef_browser_host_t> -> <int> -> Ptr <cef_string_t> -> <int> -> <int> -> <int> -> IO <void>)
#field stop_finding,          FunPtr (Ptr <cef_browser_host_t> -> <int> -> IO <void>)
#field show_dev_tools,        FunPtr (Ptr <cef_browser_host_t> -> Ptr <cef_window_info_t> -> Ptr <cef_client_t> -> Ptr <cef_browser_settings_t> -> IO <void>)
#field close_dev_tools,       FunPtr (Ptr <cef_browser_host_t> -> IO <void>)
#field set_mouse_cursor_change_disabled, FunPtr (Ptr <cef_browser_host_t> -> <int> -> IO <void>)
#field is_mouse_cursor_change_disabled, FunPtr (Ptr <cef_browser_host_t> -> IO <int>)
#field is_window_rendering_disabled, FunPtr (Ptr <cef_browser_host_t> -> IO <int>)
#field was_resized,           FunPtr (Ptr <cef_browser_host_t> -> IO <void>)
#field was_hidden,            FunPtr (Ptr <cef_browser_host_t> -> <int> -> IO <void>)
#field notify_screen_info_changed, FunPtr (Ptr <cef_browser_host_t> -> IO <void>)
#field invalidate,            FunPtr (Ptr <cef_browser_host_t> -> Ptr <cef_rect_t> -> <cef_paint_element_type_t> -> IO <void>)
#field send_key_event,        FunPtr (Ptr <cef_browser_host_t> -> Ptr <cef_key_event_t> -> IO <void>)
#field send_mouse_click_event, FunPtr (Ptr <cef_browser_host_t> -> Ptr <cef_mouse_event_t> -> <cef_mouse_button_type_t> -> <int> -> <int> -> IO <void>)
#field send_mouse_move_event, FunPtr (Ptr <cef_browser_host_t> -> Ptr <cef_mouse_event_t> -> <int> -> IO <void>)
#field send_mouse_wheel_event, FunPtr (Ptr <cef_browser_host_t> -> Ptr <cef_mouse_event_t> -> <int> -> <int> -> IO <void>)
#field send_focus_event,      FunPtr (Ptr <cef_browser_host_t> -> <int> -> IO <void>)
#field send_capture_lost_event, FunPtr (Ptr <cef_browser_host_t> -> IO <void>)
#field get_nstext_input_context, FunPtr (Ptr <cef_browser_host_t> -> IO <cef_text_input_context_t>)
#field handle_key_event_before_text_input_client, FunPtr (Ptr <cef_browser_host_t> -> <cef_event_handle_t> -> IO <void>)
#field handle_key_event_after_text_input_client, FunPtr (Ptr <cef_browser_host_t> -> <cef_event_handle_t> -> IO <void>)
#stoptype

#ccall cef_browser_host_create_browser, Ptr <cef_window_info_t> -> Ptr <cef_client_t> -> Ptr <cef_string_t> -> Ptr <cef_browser_settings_t> -> Ptr <cef_request_context_t> -> IO <int>
#ccall cef_browser_host_create_browser_sync, Ptr <cef_window_info_t> -> Ptr <cef_client_t> -> Ptr <cef_string_t> -> Ptr <cef_browser_settings_t> -> Ptr <cef_request_context_t> -> IO (Ptr <cef_browser_t>)




#starttype cef_browser_process_handler_t
#field base,                              <cef_base_t>
#field on_context_initialized,            FunPtr (Ptr <cef_browser_process_handler_t> -> IO <void>)
#field on_before_child_process_launch,    FunPtr (Ptr <cef_browser_process_handler_t> -> Ptr <cef_command_line_t> -> IO <void>)
#field on_render_process_thread_created,  FunPtr (Ptr <cef_browser_process_handler_t> -> Ptr <cef_list_value_t> -> IO <void>)
#stoptype





#starttype cef_callback_t
#field base,    <cef_base_t>
#field cont,    FunPtr (Ptr <cef_callback_t> -> IO <void>)
#field cancel,  FunPtr (Ptr <cef_callback_t> -> IO <void>)
#stoptype

#starttype cef_completion_handler_t
#field base,        <cef_base_t>
#field on_complete, FunPtr (Ptr <cef_completion_handler_t> -> IO <void>)
#stoptype





#starttype cef_client_t
#field base,                      <cef_base_t>
#field get_context_menu_handler,  FunPtr (Ptr <cef_client_t> -> IO (Ptr <cef_context_menu_handler_t>))
#field get_dialog_handler,        FunPtr (Ptr <cef_client_t> -> IO (Ptr <cef_dialog_handler_t>))
#field get_display_handler,       FunPtr (Ptr <cef_client_t> -> IO (Ptr <cef_display_handler_t>))
#field get_download_handler,      FunPtr (Ptr <cef_client_t> -> IO (Ptr <cef_download_handler_t>))
#field get_drag_handler,          FunPtr (Ptr <cef_client_t> -> IO (Ptr <cef_drag_handler_t>))
#field get_focus_handler,         FunPtr (Ptr <cef_client_t> -> IO (Ptr <cef_focus_handler_t>))
#field get_geolocation_handler,   FunPtr (Ptr <cef_client_t> -> IO (Ptr <cef_geolocation_handler_t>))
#field get_jsdialog_handler,      FunPtr (Ptr <cef_client_t> -> IO (Ptr <cef_jsdialog_handler_t>))
#field get_keyboard_handler,      FunPtr (Ptr <cef_client_t> -> IO (Ptr <cef_keyboard_handler_t>))
#field get_life_span_handler,     FunPtr (Ptr <cef_client_t> -> IO (Ptr <cef_life_span_handler_t>))
#field get_load_handler,          FunPtr (Ptr <cef_client_t> -> IO (Ptr <cef_load_handler_t>))
#field get_render_handler,        FunPtr (Ptr <cef_client_t> -> IO (Ptr <cef_render_handler_t>))
#field get_request_handler,       FunPtr (Ptr <cef_client_t> -> IO (Ptr <cef_request_handler_t>))
#field on_process_message_received, FunPtr (Ptr <cef_client_t> -> Ptr <cef_browser_t> -> <cef_process_id_t> -> Ptr <cef_process_message_t> -> IO <int>)
#stoptype





#starttype cef_command_line_t
#field base,              <cef_base_t>
#field is_valid,          FunPtr (Ptr <cef_command_line_t> -> IO <int>)
#field is_read_only,      FunPtr (Ptr <cef_command_line_t> -> IO <int>)
#field copy,              FunPtr (Ptr <cef_command_line_t> -> IO (Ptr <cef_command_line_t>))
#field init_from_argv,    FunPtr (Ptr <cef_command_line_t> -> <int> -> Ptr <string> -> IO <void>)
#field init_from_string,  FunPtr (Ptr <cef_command_line_t> -> Ptr <cef_string_t> -> IO <void>)
#field reset,             FunPtr (Ptr <cef_command_line_t> -> IO <void>)
#field get_argv,          FunPtr (Ptr <cef_command_line_t> -> <cef_string_list_t> -> IO <void>)
#field get_command_line_string, FunPtr (Ptr <cef_command_line_t> -> IO <cef_string_userfree_t>)
#field get_program,       FunPtr (Ptr <cef_command_line_t> -> IO <cef_string_userfree_t>)
#field set_program,       FunPtr (Ptr <cef_command_line_t> -> Ptr <cef_string_t> -> IO <void>)
#field has_switches,      FunPtr (Ptr <cef_command_line_t> -> IO <int>)
#field has_switch,        FunPtr (Ptr <cef_command_line_t> -> Ptr <cef_string_t> -> IO <int>)
#field get_switch_value,  FunPtr (Ptr <cef_command_line_t> -> Ptr <cef_string_t> -> IO <cef_string_userfree_t>)
#field get_switches,      FunPtr (Ptr <cef_command_line_t> -> <cef_string_map_t> -> IO <void>)
#field append_switch,     FunPtr (Ptr <cef_command_line_t> -> Ptr <cef_string_t> -> IO <void>)
#field append_switch_with_value, FunPtr (Ptr <cef_command_line_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> IO <void>)
#field has_arguments,     FunPtr (Ptr <cef_command_line_t> -> IO <int>)
#field get_arguments,     FunPtr (Ptr <cef_command_line_t> -> <cef_string_list_t> -> IO <void>)
#field append_argument,   FunPtr (Ptr <cef_command_line_t> -> Ptr <cef_string_t> -> IO <void>)
#field prepend_wrapper,   FunPtr (Ptr <cef_command_line_t> -> Ptr <cef_string_t> -> IO <void>)
#stoptype 

#ccall cef_command_line_create,     IO (Ptr <cef_command_line_t>)
#ccall cef_command_line_get_global, IO (Ptr <cef_command_line_t>)




#starttype cef_context_menu_handler_t
#field base,                      <cef_base_t>
#field on_before_context_menu,    FunPtr (Ptr <cef_context_menu_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> Ptr <cef_context_menu_params_t> -> Ptr <cef_menu_model_t> -> IO <void>)
#field on_context_menu_command,   FunPtr (Ptr <cef_context_menu_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> Ptr <cef_context_menu_params_t> -> <int> -> <cef_event_flags_t> -> IO <int>)
#field on_context_menu_dismissed, FunPtr (Ptr <cef_context_menu_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> IO <void>)
#stoptype

#starttype cef_context_menu_params_t
#field base,                <cef_base_t>
#field get_xcoord,          FunPtr (Ptr <cef_context_menu_params_t> -> IO <int>)
#field get_ycoord,          FunPtr (Ptr <cef_context_menu_params_t> -> IO <int>)
#field get_type_flags,      FunPtr (Ptr <cef_context_menu_params_t> -> IO <cef_context_menu_type_flags_t>)
#field get_link_url,        FunPtr (Ptr <cef_context_menu_params_t> -> IO <cef_string_userfree_t>)
#field get_unfiltered_link_url, FunPtr (Ptr <cef_context_menu_params_t> -> IO <cef_string_userfree_t>)
#field get_source_url,      FunPtr (Ptr <cef_context_menu_params_t> -> IO <cef_string_userfree_t>)
#field has_image_contents,  FunPtr (Ptr <cef_context_menu_params_t> -> IO <int>)
#field get_page_url,        FunPtr (Ptr <cef_context_menu_params_t> -> IO <cef_string_userfree_t>)
#field get_frame_url,       FunPtr (Ptr <cef_context_menu_params_t> -> IO <cef_string_userfree_t>)
#field get_frame_charset,   FunPtr (Ptr <cef_context_menu_params_t> -> IO <cef_string_userfree_t>)
#field get_media_type,      FunPtr (Ptr <cef_context_menu_params_t> -> IO <cef_context_menu_media_type_t>)
#field get_media_state_flags, FunPtr (Ptr <cef_context_menu_params_t> -> IO <cef_context_menu_media_state_flags_t>)
#field get_selection_text,  FunPtr (Ptr <cef_context_menu_params_t> -> IO <cef_string_userfree_t>)
#field is_editable,         FunPtr (Ptr <cef_context_menu_params_t> -> IO <int>)
#field is_speech_input_enabled, FunPtr (Ptr <cef_context_menu_params_t> -> IO <int>)
#field get_edit_state_flags, FunPtr (Ptr <cef_context_menu_params_t> -> IO <cef_context_menu_edit_state_flags_t>)
#stoptype






#starttype cef_cookie_manager_t
#field base,                  <cef_base_t>
#field set_supported_schemes, FunPtr (Ptr <cef_cookie_manager_t> -> <cef_string_list_t> -> IO <void>)
#field visit_all_cookies,     FunPtr (Ptr <cef_cookie_manager_t> -> Ptr <cef_cookie_visitor_t> -> IO <int>)
#field visit_url_cookies,     FunPtr (Ptr <cef_cookie_manager_t> -> Ptr <cef_string_t> -> <int> -> Ptr <cef_cookie_visitor_t> -> IO <int>)
#field set_cookie,            FunPtr (Ptr <cef_cookie_manager_t> -> Ptr <cef_string_t> -> Ptr <cef_cookie_t> -> IO <int>)
#field delete_cookies,        FunPtr (Ptr <cef_cookie_manager_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> IO <int>)
#field set_storage_path,      FunPtr (Ptr <cef_cookie_manager_t> -> Ptr <cef_string_t> -> <int> -> IO <int>)
#field flush_store,           FunPtr (Ptr <cef_cookie_manager_t> -> Ptr <cef_completion_handler_t> -> IO <int>)
#stoptype

#starttype cef_cookie_visitor_t
#field base,  <cef_base_t>
#field visit, FunPtr (Ptr <cef_cookie_visitor_t> -> Ptr <cef_cookie_t> -> <int> -> <int> -> Ptr <int> -> IO <int>)
#stoptype

#ccall cef_cookie_manager_get_global_manager,   IO (Ptr <cef_cookie_manager_t>)
#ccall cef_cookie_manager_create_manager,       Ptr <cef_string_t> -> <int> -> IO (Ptr <cef_cookie_manager_t>)





#starttype cef_file_dialog_callback_t
#field base,    <cef_base_t>
#field cont,    FunPtr (Ptr <cef_file_dialog_callback_t> -> <cef_string_list_t> -> IO <void>)
#field cancel,  FunPtr (Ptr <cef_file_dialog_callback_t> -> IO <void>)
#stoptype

#starttype cef_dialog_handler_t
#field base,            <cef_base_t>
#field on_file_dialog,  FunPtr (Ptr <cef_dialog_handler_t> -> Ptr <cef_browser_t> -> <cef_file_dialog_mode_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> <cef_string_list_t> -> Ptr <cef_file_dialog_callback_t> -> IO <int>)
#stoptype




#starttype cef_display_handler_t
#field base,                <cef_base_t>
#field on_address_change,   FunPtr (Ptr <cef_display_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> Ptr <cef_string_t> -> IO <void>)
#field on_title_change,     FunPtr (Ptr <cef_display_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_string_t> -> IO <void>)
#field on_tooltip,          FunPtr (Ptr <cef_display_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_string_t> -> IO <int>)
#field on_status_message,   FunPtr (Ptr <cef_display_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_string_t> -> IO <void>)
#field on_console_message,  FunPtr (Ptr <cef_display_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> <int> -> IO <int>)
#stoptype



#starttype cef_domvisitor_t
#field base,    <cef_base_t>
#field visit,   FunPtr (Ptr <cef_domvisitor_t> -> Ptr <cef_domdocument_t> -> IO <void>)
#stoptype

#starttype cef_domdocument_t
#field base,                <cef_base_t>
#field get_type,            FunPtr (Ptr <cef_domdocument_t> -> IO <cef_dom_document_type_t>)
#field get_document,        FunPtr (Ptr <cef_domdocument_t> -> IO (Ptr <cef_domnode_t>))
#field get_body,            FunPtr (Ptr <cef_domdocument_t> -> IO (Ptr <cef_domnode_t>))
#field get_head,            FunPtr (Ptr <cef_domdocument_t> -> IO (Ptr <cef_domnode_t>))
#field get_title,           FunPtr (Ptr <cef_domdocument_t> -> IO <cef_string_userfree_t>)
#field get_element_by_id,   FunPtr (Ptr <cef_domdocument_t> -> Ptr <cef_string_t> -> IO (Ptr <cef_domnode_t>))
#field get_focused_node,    FunPtr (Ptr <cef_domdocument_t> -> IO (Ptr <cef_domnode_t>))
#field has_selection,       FunPtr (Ptr <cef_domdocument_t> -> IO <int>)
#field get_selection_start_node,    FunPtr (Ptr <cef_domdocument_t> -> IO (Ptr <cef_domnode_t>))
#field get_selection_start_offset,  FunPtr (Ptr <cef_domdocument_t> -> IO <int>)
#field get_selection_end_node,      FunPtr (Ptr <cef_domdocument_t> -> IO (Ptr <cef_domnode_t>))
#field get_selection_end_offset,    FunPtr (Ptr <cef_domdocument_t> -> IO <int>)
#field get_selection_as_markup,     FunPtr (Ptr <cef_domdocument_t> -> IO <cef_string_userfree_t>)
#field get_selection_as_text,       FunPtr (Ptr <cef_domdocument_t> -> IO <cef_string_userfree_t>)
#field get_base_url,        FunPtr (Ptr <cef_domdocument_t> -> IO <cef_string_userfree_t>)
#field get_complete_url,    FunPtr (Ptr <cef_domdocument_t> -> Ptr <cef_string_t> -> IO <cef_string_userfree_t>)
#stoptype

#starttype cef_domnode_t
#field base,        <cef_base_t>
#field get_type,    FunPtr (Ptr <cef_domnode_t> -> IO <cef_dom_node_type_t>)
#field is_text,     FunPtr (Ptr <cef_domnode_t> -> IO <int>)
#field is_element,  FunPtr (Ptr <cef_domnode_t> -> IO <int>)
#field is_editable, FunPtr (Ptr <cef_domnode_t> -> IO <int>)
#field is_form_control_element,       FunPtr (Ptr <cef_domnode_t> -> IO <int>)
#field get_form_control_element_type, FunPtr (Ptr <cef_domnode_t> -> IO <cef_string_userfree_t>)
#field is_same,     FunPtr (Ptr <cef_domnode_t> -> Ptr <cef_domnode_t> -> IO <int>)
#field get_name,    FunPtr (Ptr <cef_domnode_t> -> IO <cef_string_userfree_t>)
#field get_value,   FunPtr (Ptr <cef_domnode_t> -> IO <cef_string_userfree_t>)
#field set_value,   FunPtr (Ptr <cef_domnode_t> -> Ptr <cef_string_t> -> IO <int>)
#field get_as_markup, FunPtr (Ptr <cef_domnode_t> -> IO <cef_string_userfree_t>)
#field get_document,  FunPtr (Ptr <cef_domnode_t> -> IO (Ptr <cef_domdocument_t>))
#field get_parent,    FunPtr (Ptr <cef_domnode_t> -> IO (Ptr <cef_domnode_t>))
#field get_previous_sibling,  FunPtr (Ptr <cef_domnode_t> -> IO (Ptr <cef_domnode_t>))
#field get_next_sibling,      FunPtr (Ptr <cef_domnode_t> -> IO (Ptr <cef_domnode_t>))
#field has_children,          FunPtr (Ptr <cef_domnode_t> -> IO <int>)
#field get_first_child,       FunPtr (Ptr <cef_domnode_t> -> IO (Ptr <cef_domnode_t>))
#field get_last_child,        FunPtr (Ptr <cef_domnode_t> -> IO (Ptr <cef_domnode_t>))
#field add_event_listener,    FunPtr (Ptr <cef_domnode_t> -> Ptr <cef_string_t> -> Ptr <cef_domevent_listener_t> -> <int> -> IO <void>)
#field get_element_tag_name,    FunPtr (Ptr <cef_domnode_t> -> IO <cef_string_userfree_t>)
#field has_element_attributes,  FunPtr (Ptr <cef_domnode_t> -> IO <int>)
#field has_element_attribute,   FunPtr (Ptr <cef_domnode_t> -> Ptr <cef_string_t> -> IO <int>)
#field get_element_attribute,   FunPtr (Ptr <cef_domnode_t> -> Ptr <cef_string_t> -> IO <cef_string_userfree_t>)
#field get_element_attributes,  FunPtr (Ptr <cef_domnode_t> -> <cef_string_map_t> -> IO <void>)
#field set_element_attribute,   FunPtr (Ptr <cef_domnode_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> IO <int>)
#field get_element_inner_text,  FunPtr (Ptr <cef_domnode_t> -> IO <cef_string_userfree_t>)
#stoptype

#starttype cef_domevent_t
#field base,          <cef_base_t>
#field get_type,      FunPtr (Ptr <cef_domevent_t> -> IO <cef_string_userfree_t>)
#field get_category,  FunPtr (Ptr <cef_domevent_t> -> IO <cef_dom_event_category_t>)
#field get_phase,     FunPtr (Ptr <cef_domevent_t> -> IO <cef_dom_event_phase_t>)
#field can_bubble,    FunPtr (Ptr <cef_domevent_t> -> IO <int>)
#field can_cancel,    FunPtr (Ptr <cef_domevent_t> -> IO <int>)
#field get_document,  FunPtr (Ptr <cef_domevent_t> -> IO (Ptr <cef_domdocument_t>))
#field get_target,    FunPtr (Ptr <cef_domevent_t> -> IO (Ptr <cef_domnode_t>))
#field get_current_target, FunPtr (Ptr <cef_domevent_t> -> IO (Ptr <cef_domnode_t>))
#stoptype

#starttype cef_domevent_listener_t
#field base,          <cef_base_t>
#field handle_event,  FunPtr (Ptr <cef_domevent_listener_t> -> Ptr <cef_domevent_t> -> IO <void>)
#stoptype





#starttype cef_before_download_callback_t
#field base,  <cef_base_t>
#field cont,  FunPtr (Ptr <cef_string_t> -> <int> -> IO <void>)
#stoptype

#starttype cef_download_item_callback_t
#field base,    <cef_base_t>
#field cancel,  FunPtr (Ptr <cef_download_item_callback_t> -> IO <void>)
#stoptype

#starttype cef_download_handler_t
#field base,                <cef_base_t>
#field on_before_download,  FunPtr (Ptr <cef_download_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_download_item_t> -> Ptr <cef_string_t> -> Ptr <cef_before_download_callback_t> -> IO <void>)
#field on_download_updated, FunPtr (Ptr <cef_download_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_download_item_t> -> Ptr <cef_download_item_callback_t> -> IO <void>)
#stoptype





#starttype cef_download_item_t
#field base,                    <cef_base_t>
#field is_valid,                FunPtr (Ptr <cef_download_item_t> -> IO <int>)
#field is_in_progress,          FunPtr (Ptr <cef_download_item_t> -> IO <int>)
#field is_complete,             FunPtr (Ptr <cef_download_item_t> -> IO <int>)
#field is_canceled,             FunPtr (Ptr <cef_download_item_t> -> IO <int>)
#field get_current_speed,       FunPtr (Ptr <cef_download_item_t> -> IO <int64>)
#field get_percent_complete,    FunPtr (Ptr <cef_download_item_t> -> IO <int>)
#field get_total_bytes,         FunPtr (Ptr <cef_download_item_t> -> IO <int64>)
#field get_received_bytes,      FunPtr (Ptr <cef_download_item_t> -> IO <int64>)
#field get_start_time,          FunPtr (Ptr <cef_download_item_t> -> IO <cef_time_t>)
#field get_end_time,            FunPtr (Ptr <cef_download_item_t> -> IO <cef_time_t>)
#field get_full_path,           FunPtr (Ptr <cef_download_item_t> -> IO <cef_string_userfree_t>)
#field get_id,                  FunPtr (Ptr <cef_download_item_t> -> IO <uint32>)
#field get_url,                 FunPtr (Ptr <cef_download_item_t> -> IO <cef_string_userfree_t>)
#field get_suggested_file_name, FunPtr (Ptr <cef_download_item_t> -> IO <cef_string_userfree_t>)
#field get_content_disposition, FunPtr (Ptr <cef_download_item_t> -> IO <cef_string_userfree_t>)
#field get_mime_type,           FunPtr (Ptr <cef_download_item_t> -> IO <cef_string_userfree_t>)
#stoptype





#starttype cef_drag_data_t
#field base,                <cef_base_t>
#field is_link,             FunPtr (Ptr <cef_drag_data_t> -> IO <int>)
#field is_fragment,         FunPtr (Ptr <cef_drag_data_t> -> IO <int>)
#field is_file,             FunPtr (Ptr <cef_drag_data_t> -> IO <int>)
#field get_link_url,        FunPtr (Ptr <cef_drag_data_t> -> IO <cef_string_userfree_t>)
#field get_link_title,      FunPtr (Ptr <cef_drag_data_t> -> IO <cef_string_userfree_t>)
#field get_link_metadata,   FunPtr (Ptr <cef_drag_data_t> -> IO <cef_string_userfree_t>)
#field get_fragment_text,   FunPtr (Ptr <cef_drag_data_t> -> IO <cef_string_userfree_t>)
#field get_fragment_html,   FunPtr (Ptr <cef_drag_data_t> -> IO <cef_string_userfree_t>)
#field get_fragment_base_url, FunPtr (Ptr <cef_drag_data_t> -> IO <cef_string_userfree_t>)
#field get_file_name,       FunPtr (Ptr <cef_drag_data_t> -> IO <cef_string_userfree_t>)
#field get_file_names,      FunPtr (Ptr <cef_drag_data_t> -> <cef_string_list_t> -> IO <int>)
#stoptype





#starttype cef_drag_handler_t
#field base,          <cef_base_t>
#field on_drag_enter, FunPtr (Ptr <cef_drag_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_drag_data_t> -> <cef_drag_operations_mask_t> -> IO <int>)
#stoptype




#starttype cef_focus_handler_t
#field base,            <cef_base_t>
#field on_take_focus,   FunPtr (Ptr <cef_focus_handler_t> -> Ptr <cef_browser_t> -> <int> -> IO <void>)
#field on_set_focus,    FunPtr (Ptr <cef_focus_handler_t> -> Ptr <cef_browser_t> -> <cef_focus_source_t> -> IO <int>)
#field on_got_focus,    FunPtr (Ptr <cef_focus_handler_t> -> Ptr <cef_browser_t> -> IO <void>)
#stoptype




#starttype cef_frame_t
#field base,          <cef_base_t>
#field is_valid,      FunPtr (Ptr <cef_frame_t> -> IO <int>)
#field undo,          FunPtr (Ptr <cef_frame_t> -> IO <void>)
#field redo,          FunPtr (Ptr <cef_frame_t> -> IO <void>)
#field cut,           FunPtr (Ptr <cef_frame_t> -> IO <void>)
#field copy,          FunPtr (Ptr <cef_frame_t> -> IO <void>)
#field paste,         FunPtr (Ptr <cef_frame_t> -> IO <void>)
#field del,           FunPtr (Ptr <cef_frame_t> -> IO <void>)
#field select_all,    FunPtr (Ptr <cef_frame_t> -> IO <void>)
#field view_source,   FunPtr (Ptr <cef_frame_t> -> IO <void>)
#field get_source,    FunPtr (Ptr <cef_frame_t> -> Ptr <cef_string_visitor_t> -> IO <void>)
#field get_text,      FunPtr (Ptr <cef_frame_t> -> Ptr <cef_string_visitor_t> -> IO <void>)
#field load_request,  FunPtr (Ptr <cef_frame_t> -> Ptr <cef_request_t> -> IO <void>)
#field load_url,      FunPtr (Ptr <cef_frame_t> -> Ptr <cef_string_t> -> IO <void>)
#field load_string,   FunPtr (Ptr <cef_frame_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> IO <void>)
#field execute_java_script, FunPtr (Ptr <cef_frame_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> <int> -> IO <void>)
#field is_main,       FunPtr (Ptr <cef_frame_t> -> IO <int>)
#field is_focused,    FunPtr (Ptr <cef_frame_t> -> IO <int>)
#field get_name,      FunPtr (Ptr <cef_frame_t> -> IO <cef_string_userfree_t>)
#field get_identifier,  FunPtr (Ptr <cef_frame_t> -> IO <int64>)
#field get_parent,    FunPtr (Ptr <cef_frame_t> -> IO (Ptr <cef_frame_t>))
#field get_url,       FunPtr (Ptr <cef_frame_t> -> IO <cef_string_userfree_t>)
#field get_browser,   FunPtr (Ptr <cef_frame_t> -> IO (Ptr <cef_browser_t>))
#field get_v8context, FunPtr (Ptr <cef_frame_t> -> IO (Ptr <cef_v8context_t>))
#field visit_dom,     FunPtr (Ptr <cef_frame_t> -> Ptr <cef_domvisitor_t> -> IO <void>)
#stoptype





#starttype cef_get_geolocation_callback_t
#field base,                <cef_base_t>
#field on_location_update,  FunPtr (Ptr <cef_get_geolocation_callback_t> -> Ptr <cef_geoposition_t> -> IO <void>)
#stoptype


#ccall cef_get_geolocation, Ptr <cef_get_geolocation_callback_t> -> IO <int>



#starttype cef_geolocation_callback_t
#field base,  <cef_base_t>
#field cont,  FunPtr (Ptr <cef_geolocation_callback_t> -> <int> -> IO <void>)
#stoptype

#starttype cef_geolocation_handler_t
#field base,                              <cef_base_t>
#field on_request_geolocation_permission, FunPtr (Ptr <cef_geolocation_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_string_t> -> <int> -> Ptr <cef_geolocation_callback_t> -> IO <void>)
#field on_cancel_geolocation_permission,  FunPtr (Ptr <cef_geolocation_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_string_t> -> <int> -> IO <void>)
#stoptype




#starttype cef_jsdialog_callback_t
#field base,  <cef_base_t>
#field cont,  FunPtr (Ptr <cef_jsdialog_callback_t> -> <int> -> Ptr <cef_string_t> -> IO <void>)
#stoptype

#starttype cef_jsdialog_handler_t
#field base,                <cef_base_t>
#field on_jsdialog,         FunPtr (Ptr <cef_jsdialog_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> <cef_jsdialog_type_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> Ptr <cef_jsdialog_callback_t> -> Ptr <int> -> IO <int>)
#field on_before_unload_dialog, FunPtr (Ptr <cef_jsdialog_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_string_t> -> <int> -> Ptr <cef_jsdialog_callback_t> -> IO <int>)
#field on_reset_dialog_state,   FunPtr (Ptr <cef_jsdialog_handler_t> -> Ptr <cef_browser_t> -> IO <void>)
#field on_dialog_closed,        FunPtr (Ptr <cef_jsdialog_handler_t> -> Ptr <cef_browser_t> -> IO <void>)
#stoptype





#starttype cef_keyboard_handler_t
#field base,              <cef_base_t>
#field on_pre_key_event,  FunPtr (Ptr <cef_keyboard_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_key_event_t> -> <cef_event_handle_t> -> Ptr <int> -> IO <int>)
#field on_key_event,      FunPtr (Ptr <cef_keyboard_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_key_event_t> -> <cef_event_handle_t> -> IO <int>)
#stoptype





#starttype cef_life_span_handler_t
#field base,                <cef_base_t>
#field on_before_popup,     FunPtr (Ptr <cef_life_span_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> Ptr <cef_popup_features_t> -> Ptr <cef_window_info_t> -> Ptr (Ptr <cef_client_t>) -> Ptr <cef_browser_settings_t> -> Ptr <int> -> IO <int>)
#field on_after_created,    FunPtr (Ptr <cef_life_span_handler_t> -> Ptr <cef_browser_t> -> IO <void>)
#field run_modal,           FunPtr (Ptr <cef_life_span_handler_t> -> Ptr <cef_browser_t> -> IO <int>)
#field do_close,            FunPtr (Ptr <cef_life_span_handler_t> -> Ptr <cef_browser_t> -> IO <int>)
#field on_before_close,     FunPtr (Ptr <cef_life_span_handler_t> -> Ptr <cef_browser_t> -> IO <void>)
#stoptype





#starttype cef_load_handler_t
#field base,                      <cef_base_t>
#field on_loading_state_change,   FunPtr (Ptr <cef_load_handler_t> -> Ptr <cef_browser_t> -> <int> -> <int> -> <int> -> IO <void>)
#field on_load_start,             FunPtr (Ptr <cef_load_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> IO <void>)
#field on_load_end,               FunPtr (Ptr <cef_load_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> <int> -> IO <void>)
#field on_load_error,             FunPtr (Ptr <cef_load_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> <cef_errorcode_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> IO <void>)
#stoptype





#starttype cef_menu_model_t
#field base,                  <cef_base_t>
#field clear,                 FunPtr (Ptr <cef_menu_model_t> -> IO <int>)
#field get_count,             FunPtr (Ptr <cef_menu_model_t> -> IO <int>)
#field add_separator,         FunPtr (Ptr <cef_menu_model_t> -> IO <int>)
#field add_item,              FunPtr (Ptr <cef_menu_model_t> -> <int> -> Ptr <cef_string_t> -> IO <int>)
#field add_check_item,        FunPtr (Ptr <cef_menu_model_t> -> <int> -> Ptr <cef_string_t> -> IO <int>)
#field add_radio_item,        FunPtr (Ptr <cef_menu_model_t> -> <int> -> Ptr <cef_string_t> -> <int> -> IO <int>)
#field add_sub_menu,          FunPtr (Ptr <cef_menu_model_t> -> <int> -> Ptr <cef_string_t> -> IO (Ptr <cef_menu_model_t>))
#field insert_separator_at,   FunPtr (Ptr <cef_menu_model_t> -> <int> -> IO <int>)
#field insert_item_at,        FunPtr (Ptr <cef_menu_model_t> -> <int> -> <int> -> Ptr <cef_string_t> -> IO <int>)
#field insert_check_item_at,  FunPtr (Ptr <cef_menu_model_t> -> <int> -> <int> -> Ptr <cef_string_t> -> IO <int>)
#field insert_radio_item_at,  FunPtr (Ptr <cef_menu_model_t> -> <int> -> <int> -> Ptr <cef_string_t> -> <int> -> IO <int>)
#field insert_sub_menu_at,    FunPtr (Ptr <cef_menu_model_t> -> <int> -> <int> -> Ptr <cef_string_t> -> IO (Ptr <cef_menu_model_t>))
#field remove,                FunPtr (Ptr <cef_menu_model_t> -> <int> -> IO <int>)
#field remove_at,             FunPtr (Ptr <cef_menu_model_t> -> <int> -> IO <int>)
#field get_index_of,          FunPtr (Ptr <cef_menu_model_t> -> <int> -> IO <int>)
#field get_command_id_at,     FunPtr (Ptr <cef_menu_model_t> -> <int> -> IO <int>)
#field set_command_id_at,     FunPtr (Ptr <cef_menu_model_t> -> <int> -> <int> -> IO <int>)
#field get_label,             FunPtr (Ptr <cef_menu_model_t> -> <int> -> IO <cef_string_userfree_t>)
#field get_label_at,          FunPtr (Ptr <cef_menu_model_t> -> <int> -> IO <cef_string_userfree_t>)
#field set_label,             FunPtr (Ptr <cef_menu_model_t> -> <int> -> Ptr <cef_string_t> -> IO <int>)
#field set_label_at,          FunPtr (Ptr <cef_menu_model_t> -> <int> -> Ptr <cef_string_t> -> IO <int>)
#field get_type,              FunPtr (Ptr <cef_menu_model_t> -> <int> -> IO <cef_menu_item_type_t>)
#field get_type_at,           FunPtr (Ptr <cef_menu_model_t> -> <int> -> IO <cef_menu_item_type_t>)
#field get_group_id,          FunPtr (Ptr <cef_menu_model_t> -> <int> -> IO <int>)
#field get_group_id_at,       FunPtr (Ptr <cef_menu_model_t> -> <int> -> IO <int>)
#field set_group_id,          FunPtr (Ptr <cef_menu_model_t> -> <int> -> <int> -> IO <int>)
#field set_group_id_at,       FunPtr (Ptr <cef_menu_model_t> -> <int> -> <int> -> IO <int>)
#field get_sub_menu,          FunPtr (Ptr <cef_menu_model_t> -> <int> -> IO (Ptr <cef_menu_model_t>))
#field get_sub_menu_at,       FunPtr (Ptr <cef_menu_model_t> -> <int> -> IO (Ptr <cef_menu_model_t>))
#field is_visible,            FunPtr (Ptr <cef_menu_model_t> -> <int> -> IO <int>)
#field is_visible_at,         FunPtr (Ptr <cef_menu_model_t> -> <int> -> IO <int>)
#field set_visible,           FunPtr (Ptr <cef_menu_model_t> -> <int> -> <int> -> IO <int>)
#field set_visible_at,        FunPtr (Ptr <cef_menu_model_t> -> <int> -> <int> -> IO <int>)
#field is_enabled,            FunPtr (Ptr <cef_menu_model_t> -> <int> -> IO <int>)
#field is_enabled_at,         FunPtr (Ptr <cef_menu_model_t> -> <int> -> IO <int>)
#field set_enabled,           FunPtr (Ptr <cef_menu_model_t> -> <int> -> <int> -> IO <int>)
#field set_enabled_at,        FunPtr (Ptr <cef_menu_model_t> -> <int> -> <int> -> IO <int>)
#field is_checked,            FunPtr (Ptr <cef_menu_model_t> -> <int> -> IO <int>)
#field is_checked_at,         FunPtr (Ptr <cef_menu_model_t> -> <int> -> IO <int>)
#field set_checked,           FunPtr (Ptr <cef_menu_model_t> -> <int> -> <int> -> IO <int>)
#field set_checked_at,        FunPtr (Ptr <cef_menu_model_t> -> <int> -> <int> -> IO <int>)
#field has_accelerator,       FunPtr (Ptr <cef_menu_model_t> -> <int> -> IO <int>)
#field has_accelerator_at,    FunPtr (Ptr <cef_menu_model_t> -> <int> -> IO <int>)
#field set_accelerator,       FunPtr (Ptr <cef_menu_model_t> -> <int> -> <int> -> <int> -> <int> -> <int> -> IO <int>)
#field set_accelerator_at,    FunPtr (Ptr <cef_menu_model_t> -> <int> -> <int> -> <int> -> <int> -> <int> -> IO <int>)
#field remove_accelerator,    FunPtr (Ptr <cef_menu_model_t> -> <int> -> IO <int>)
#field remove_accelerator_at, FunPtr (Ptr <cef_menu_model_t> -> <int> -> IO <int>)
#field get_accelerator,       FunPtr (Ptr <cef_menu_model_t> -> <int> -> Ptr <int> -> Ptr <int> -> Ptr <int> -> Ptr <int> -> IO <int>)
#field get_accelerator_at,    FunPtr (Ptr <cef_menu_model_t> -> <int> -> Ptr <int> -> Ptr <int> -> Ptr <int> -> Ptr <int> -> IO <int>)
#stoptype




#ccall cef_add_cross_origin_whitelist_entry,    Ptr <cef_string_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> <int> -> IO <int>
#ccall cef_remove_cross_origin_whitelist_entry, Ptr <cef_string_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> <int> -> IO <int>
#ccall cef_clear_cross_origin_whitelist,        IO <int>




#ccall cef_get_path, <cef_path_key_t> -> Ptr <cef_string_t> -> IO <int>




#starttype cef_process_message_t
#field base,                <cef_base_t>
#field is_valid,            FunPtr (Ptr <cef_process_message_t> -> IO <int>)
#field is_read_only,        FunPtr (Ptr <cef_process_message_t> -> IO <int>)
#field copy,                FunPtr (Ptr <cef_process_message_t> -> IO (Ptr <cef_process_message_t>))
#field get_name,            FunPtr (Ptr <cef_process_message_t> -> IO <cef_string_userfree_t>)
#field get_argument_list,   FunPtr (Ptr <cef_process_message_t> -> IO (Ptr <cef_list_value_t>))
#stoptype

#ccall cef_process_message_create, Ptr <cef_string_t> -> IO (Ptr <cef_process_message_t>)





#ccall cef_launch_process, Ptr <cef_command_line_t> -> IO <int>





#starttype cef_render_handler_t
#field base,                  <cef_base_t>
#field get_root_screen_rect,  FunPtr (Ptr <cef_render_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_rect_t> -> IO <int>)
#field get_view_rect,         FunPtr (Ptr <cef_render_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_rect_t> -> IO <int>)
#field get_screen_point,      FunPtr (Ptr <cef_render_handler_t> -> Ptr <cef_browser_t> -> <int> -> <int> -> Ptr <int> -> Ptr <int> -> IO <int>)
#field get_screen_info,       FunPtr (Ptr <cef_render_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_screen_info_t> -> IO <int>)
#field on_popup_show,         FunPtr (Ptr <cef_render_handler_t> -> Ptr <cef_browser_t> -> <int> -> IO <void>)
#field on_popup_size,         FunPtr (Ptr <cef_render_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_rect_t> -> IO <void>)
#field on_paint,              FunPtr (Ptr <cef_render_handler_t> -> Ptr <cef_browser_t> -> <cef_paint_element_type_t> -> <size_t> -> Ptr <cef_rect_t> -> Ptr <void> -> <int> -> <int> -> IO <void>)
#field on_cursor_change,      FunPtr (Ptr <cef_render_handler_t> -> Ptr <cef_browser_t> -> <cef_cursor_handle_t> -> IO <void>)
#field on_scroll_offset_changed, FunPtr (Ptr <cef_render_handler_t> -> Ptr <cef_browser_t> -> IO <void>)
#stoptype




#starttype cef_render_process_handler_t
#field base,                        <cef_base_t>
#field on_render_thread_created,    FunPtr (Ptr <cef_render_process_handler_t> -> Ptr <cef_list_value_t> -> IO <void>)
#field on_web_kit_initialized,      FunPtr (Ptr <cef_render_process_handler_t> -> IO <void>)
#field on_browser_created,          FunPtr (Ptr <cef_render_process_handler_t> -> Ptr <cef_browser_t> -> IO <void>)
#field on_browser_destroyed,        FunPtr (Ptr <cef_render_process_handler_t> -> Ptr <cef_browser_t> -> IO <void>)
#field get_load_handler,            FunPtr (Ptr <cef_render_process_handler_t> -> IO (Ptr <cef_load_handler_t>))
#field on_before_navigation,        FunPtr (Ptr <cef_render_process_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> Ptr <cef_request_t> -> <cef_navigation_type_t> -> <int> -> IO <int>)
#field on_context_created,          FunPtr (Ptr <cef_render_process_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> Ptr <cef_v8context_t> -> IO <void>)
#field on_context_released,         FunPtr (Ptr <cef_render_process_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> Ptr <cef_v8context_t> -> IO <void>)
#field on_uncaught_exception,       FunPtr (Ptr <cef_render_process_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> Ptr <cef_v8context_t> -> Ptr <cef_v8exception_t> -> Ptr <cef_v8stack_trace_t> -> IO <void>)
#field on_focused_node_changed,     FunPtr (Ptr <cef_render_process_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> Ptr <cef_domnode_t> -> IO <void>)
#field on_process_message_received, FunPtr (Ptr <cef_render_process_handler_t> -> Ptr <cef_browser_t> -> <cef_process_id_t> -> Ptr <cef_process_message_t> -> IO <int>)
#stoptype





#starttype cef_request_context_t
#field base,        <cef_base_t>
#field is_same,     FunPtr (Ptr <cef_request_context_t> -> Ptr <cef_request_context_t> -> IO <int>)
#field is_global,   FunPtr (Ptr <cef_request_context_t> -> IO <int>)
#field get_handler, FunPtr (Ptr <cef_request_context_t> -> IO (Ptr <cef_request_context_handler_t>))
#stoptype

#ccall cef_request_create, IO (Ptr <cef_request_t>)
#ccall cef_post_data_create, IO (Ptr <cef_post_data_t>)
#ccall cef_post_data_element_create, IO (Ptr <cef_post_data_element_t>)




#starttype cef_request_context_handler_t
#field base,                <cef_base_t>
#field get_cookie_manager,  FunPtr (Ptr <cef_request_context_handler_t> -> IO (Ptr <cef_cookie_manager_t>))
#stoptype

#ccall cef_request_context_get_global_context,  IO (Ptr <cef_request_context_t>)
#ccall cef_request_context_create_context,      IO (Ptr <cef_request_context_t>) 





#starttype cef_quota_callback_t
#field base,    <cef_base_t>
#field cont,    FunPtr (Ptr <cef_quota_callback_t> -> <int> -> IO <void>)
#field cancel,  FunPtr (Ptr <cef_quota_callback_t> -> IO <void>)
#stoptype


#starttype cef_allow_certificate_error_callback_t
#field base,    <cef_base_t>
#field cont,    FunPtr (Ptr <cef_allow_certificate_error_callback_t> -> <int> -> IO <void>)
#stoptype


#starttype cef_request_handler_t
#field base,                    <cef_base_t>
#field on_before_browse,        FunPtr (Ptr <cef_request_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> Ptr <cef_request_t> -> <int> -> IO <int>)
#field on_before_resource_load, FunPtr (Ptr <cef_request_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> Ptr <cef_request_t> -> IO <int>)
#field get_resource_handler,    FunPtr (Ptr <cef_request_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> Ptr <cef_request_t> -> IO (Ptr <cef_resource_handler_t>))
#field on_resource_redirect,    FunPtr (Ptr <cef_request_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> IO <void>)
#field get_auth_credentials,    FunPtr (Ptr <cef_request_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> <int> -> Ptr <cef_string_t> -> <int> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> Ptr <cef_auth_callback_t> -> IO <int>)
#field on_quota_request,        FunPtr (Ptr <cef_request_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_string_t> -> <int64> -> Ptr <cef_quota_callback_t> -> IO <int>)
#field on_protocol_execution,   FunPtr (Ptr <cef_request_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_string_t> -> Ptr <int> -> IO <void>)
#field on_certificate_error,    FunPtr (Ptr <cef_request_handler_t> -> <cef_errorcode_t> -> Ptr <cef_string_t> -> Ptr <cef_allow_certificate_error_callback_t> -> IO <int>)
#field on_before_plugin_load,   FunPtr (Ptr <cef_request_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> Ptr <cef_web_plugin_info_t> -> IO <int>)
#field on_plugin_crashed,       FunPtr (Ptr <cef_request_handler_t> -> Ptr <cef_browser_t> -> Ptr <cef_string_t> -> IO <void>)
#field on_render_process_terminated, FunPtr (Ptr <cef_request_handler_t> -> Ptr <cef_browser_t> -> <cef_termination_status_t> -> IO <void>)
#stoptype




#starttype cef_request_t
#field base,                <cef_base_t>
#field is_read_only,        FunPtr (Ptr <cef_request_t> -> IO <int>)
#field get_url,             FunPtr (Ptr <cef_request_t> -> IO <cef_string_userfree_t>)
#field set_url,             FunPtr (Ptr <cef_request_t> -> Ptr <cef_string_t> -> IO <void>)
#field get_method,          FunPtr (Ptr <cef_request_t> -> IO <cef_string_userfree_t>)
#field set_method,          FunPtr (Ptr <cef_request_t> -> Ptr <cef_string_t> -> IO <void>)
#field get_post_data,       FunPtr (Ptr <cef_request_t> -> IO (Ptr <cef_post_data_t>))
#field set_post_data,       FunPtr (Ptr <cef_request_t> -> Ptr <cef_post_data_t> -> IO <void>)
#field get_header_map,      FunPtr (Ptr <cef_request_t> -> <cef_string_multimap_t> -> IO <void>)
#field set_header_map,      FunPtr (Ptr <cef_request_t> -> <cef_string_multimap_t> -> IO <void>)
#field set,                 FunPtr (Ptr <cef_request_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> Ptr <cef_post_data_t> -> <cef_string_multimap_t> -> IO <void>)
#field get_flags,           FunPtr (Ptr <cef_request_t> -> IO <int>)
#field set_flags,           FunPtr (Ptr <cef_request_t> -> <int> -> IO <void>)
#field get_first_party_for_cookies, FunPtr (Ptr <cef_request_t> -> IO <cef_string_userfree_t>)
#field set_first_party_for_cookies, FunPtr (Ptr <cef_request_t> -> Ptr <cef_string_userfree_t> -> IO <void>)
#field get_resource_type,   FunPtr (Ptr <cef_request_t> -> IO <cef_resource_type_t>)
#field get_transition_type, FunPtr (Ptr <cef_request_t> -> IO <cef_transition_type_t>)
#stoptype

#starttype cef_post_data_t
#field base,                <cef_base_t>
#field is_read_only,        FunPtr (Ptr <cef_post_data_t> -> IO <int>)
#field get_element_count,   FunPtr (Ptr <cef_post_data_t> -> IO <size_t>)
#field get_elements,        FunPtr (Ptr <cef_post_data_t> -> Ptr <size_t> -> Ptr (Ptr <cef_post_data_element_t>) -> IO <void>)
#field remove_element,      FunPtr (Ptr <cef_post_data_t> -> Ptr <cef_post_data_element_t> -> IO <int>)
#field add_element,         FunPtr (Ptr <cef_post_data_t> -> Ptr <cef_post_data_element_t> -> IO <int>)
#field remove_elements,     FunPtr (Ptr <cef_post_data_t> -> IO <void>)
#stoptype

#starttype cef_post_data_element_t
#field base,                <cef_base_t>
#field is_read_only,        FunPtr (Ptr <cef_post_data_element_t> -> IO <int>)
#field set_to_empty,        FunPtr (Ptr <cef_post_data_element_t> -> IO <void>)
#field set_to_file,         FunPtr (Ptr <cef_post_data_element_t> -> Ptr <cef_string_t> -> IO <void>)
#field set_to_bytes,        FunPtr (Ptr <cef_post_data_element_t> -> <size_t> -> Ptr <void> -> IO <void>)
#field get_type,            FunPtr (Ptr <cef_post_data_element_t> -> IO <cef_postdataelement_type_t>)
#field get_file,            FunPtr (Ptr <cef_post_data_element_t> -> IO <cef_string_userfree_t>)
#field get_bytes_count,     FunPtr (Ptr <cef_post_data_element_t> -> IO <size_t>)
#field get_bytes,           FunPtr (Ptr <cef_post_data_element_t> -> <size_t> -> Ptr <void> -> IO <size_t>)
#stoptype




#starttype cef_resource_bundle_handler_t
#field base,                  <cef_base_t>
#field get_localized_string,  FunPtr (Ptr <cef_resource_bundle_handler_t> -> <int> -> Ptr <cef_string_t> -> IO <int>)
#field get_data_resource,     FunPtr (Ptr <cef_resource_bundle_handler_t> -> <int> -> Ptr (Ptr <void>) -> Ptr <size_t> -> IO <int>)
#stoptype




#starttype cef_resource_handler_t
#field base,                      <cef_base_t>
#field process_request,           FunPtr (Ptr <cef_resource_handler_t> -> Ptr <cef_request_t> -> Ptr <cef_callback_t> -> IO <int>)
#field get_response_headers,      FunPtr (Ptr <cef_resource_handler_t> -> Ptr <cef_response_t> -> Ptr <int64> -> Ptr <cef_string_t> -> IO <void>)
#field read_response,             FunPtr (Ptr <cef_resource_handler_t> -> Ptr <void> -> <int> -> Ptr <int> -> Ptr <cef_callback_t> -> IO <int>)
#field can_get_cookie,            FunPtr (Ptr <cef_resource_handler_t> -> Ptr <cef_cookie_t> -> IO <int>)
#field can_set_cookie,            FunPtr (Ptr <cef_resource_handler_t> -> Ptr <cef_cookie_t> -> IO <int>)
#field cancel,                    FunPtr (Ptr <cef_resource_handler_t> -> IO <void>)
#stoptype





#starttype cef_response_t
#field base,                <cef_base_t>
#field is_read_only,        FunPtr (Ptr <cef_response_t> -> IO <int>)
#field get_status,          FunPtr (Ptr <cef_response_t> -> IO <int>)
#field set_status,          FunPtr (Ptr <cef_response_t> -> <int> -> IO <void>)
#field get_status_text,     FunPtr (Ptr <cef_response_t> -> IO <cef_string_userfree_t>)
#field set_status_text,     FunPtr (Ptr <cef_response_t> -> Ptr <cef_string_t> -> IO <void>)
#field get_mime_type,       FunPtr (Ptr <cef_response_t> -> IO <cef_string_userfree_t>)
#field set_mime_type,       FunPtr (Ptr <cef_response_t> -> Ptr <cef_string_t> -> IO <void>)
#field get_header,          FunPtr (Ptr <cef_response_t> -> Ptr <cef_string_t> -> IO <cef_string_userfree_t>)
#field get_header_map,      FunPtr (Ptr <cef_response_t> -> <cef_string_multimap_t> -> IO <void>)
#field set_header_map,      FunPtr (Ptr <cef_response_t> -> <cef_string_multimap_t> -> IO <void>)
#stoptype

#ccall cef_response_create, IO (Ptr <cef_response_t>)





#starttype cef_scheme_registrar_t
#field base,                <cef_base_t>
#field add_custom_scheme,   FunPtr (Ptr <cef_scheme_registrar_t> -> Ptr <cef_string_t> -> <int> -> <int> -> <int> -> IO <int>)
#stoptype

#starttype cef_scheme_handler_factory_t
#field base,    <cef_base_t>
#field create,  FunPtr (Ptr <cef_scheme_handler_factory_t> -> Ptr <cef_browser_t> -> Ptr <cef_frame_t> -> Ptr <cef_string_t> -> Ptr <cef_request_t> -> IO (Ptr <cef_resource_handler_t>))
#stoptype

#ccall cef_register_scheme_handler_factory, Ptr <cef_string_t> -> Ptr <cef_string_t> -> Ptr <cef_scheme_handler_factory_t> -> IO <int>
#ccall cef_clear_scheme_handler_factories,  IO <int>






#starttype cef_read_handler_t
#field base,      <cef_base_t>
#field read,      FunPtr (Ptr <cef_read_handler_t> -> Ptr <void> -> <size_t> -> <size_t> -> IO <size_t>)
#field seek,      FunPtr (Ptr <cef_read_handler_t> -> <int64> -> <int> -> IO <int>)
#field tell,      FunPtr (Ptr <cef_read_handler_t> -> IO <int64>)
#field eof,       FunPtr (Ptr <cef_read_handler_t> -> IO <int>)
#field may_block, FunPtr (Ptr <cef_read_handler_t> -> IO <int>)
#stoptype



#starttype cef_stream_reader_t
#field base,      <cef_base_t>
#field read,      FunPtr (Ptr <cef_stream_reader_t> -> Ptr <void> -> <size_t> -> <size_t> -> IO <size_t>)
#field seek,      FunPtr (Ptr <cef_stream_reader_t> -> <int64> -> <int> -> IO <int>)
#field tell,      FunPtr (Ptr <cef_stream_reader_t> -> IO <int64>)
#field eof,       FunPtr (Ptr <cef_stream_reader_t> -> IO <int>)
#field may_block, FunPtr (Ptr <cef_stream_reader_t> -> IO <int>)
#stoptype



#starttype cef_write_handler_t
#field base,      <cef_base_t>
#field write,     FunPtr (Ptr <cef_write_handler_t> -> Ptr <void> -> <size_t> -> <size_t> -> IO <size_t>)
#field seek,      FunPtr (Ptr <cef_write_handler_t> -> <int64> -> <int> -> IO <int>)
#field tell,      FunPtr (Ptr <cef_write_handler_t> -> IO <int64>)
#field flush,     FunPtr (Ptr <cef_write_handler_t> -> IO <int>)
#field may_block, FunPtr (Ptr <cef_write_handler_t> -> IO <int>)
#stoptype



#starttype cef_stream_writer_t
#field base,      <cef_base_t>
#field write,     FunPtr (Ptr <cef_stream_writer_t> -> Ptr <void> -> <size_t> -> <size_t> -> IO <size_t>)
#field seek,      FunPtr (Ptr <cef_stream_writer_t> -> <int64> -> <int> -> IO <int>)
#field tell,      FunPtr (Ptr <cef_stream_writer_t> -> IO <int64>)
#field flush,     FunPtr (Ptr <cef_stream_writer_t> -> IO <int>)
#field may_block, FunPtr (Ptr <cef_stream_writer_t> -> IO <int>)
#stoptype

#ccall cef_stream_reader_create_for_file,     Ptr <cef_string_t> -> IO (Ptr <cef_stream_reader_t>)
#ccall cef_stream_reader_create_for_data,     Ptr <void> -> <size_t> -> IO (Ptr <cef_stream_reader_t>)
#ccall cef_stream_reader_create_for_handler,  Ptr <cef_read_handler_t> -> IO (Ptr <cef_stream_reader_t>)

#ccall cef_stream_writer_create_for_file,     Ptr <cef_string_t> -> IO (Ptr <cef_stream_writer_t>)
#ccall cef_stream_writer_create_for_handler,  Ptr <cef_write_handler_t> -> IO (Ptr <cef_stream_writer_t>)





#starttype cef_string_visitor_t
#field base,  <cef_base_t>
#field visit, FunPtr (Ptr <cef_string_visitor_t> -> Ptr <cef_string_t> -> IO <void>)
#stoptype





#starttype cef_task_t
#field base,    <cef_base_t>
#field execute, FunPtr (Ptr <cef_task_t> -> IO <void>)
#stoptype

#starttype cef_task_runner_t
#field base,                      <cef_base_t>
#field is_same,                   FunPtr (Ptr <cef_task_runner_t> -> Ptr <cef_task_runner_t> -> IO <int>)
#field belongs_to_current_thread, FunPtr (Ptr <cef_task_runner_t> -> IO <int>)
#field belongs_to_thread,         FunPtr (Ptr <cef_task_runner_t> -> <cef_thread_id_t> -> IO <int>)
#field post_task,                 FunPtr (Ptr <cef_task_runner_t> -> Ptr <cef_task_t> -> IO <int>)
#field post_delayed_task,         FunPtr (Ptr <cef_task_runner_t> -> Ptr <cef_task_t> -> <int64> -> IO <int>)
#stoptype

#ccall cef_task_runner_get_for_current_thread,  IO (Ptr <cef_task_runner_t>)
#ccall cef_task_runner_get_for_thread,          IO (Ptr <cef_task_runner_t>)
#ccall cef_currently_on,                        <cef_thread_id_t> -> IO <int>
#ccall cef_post_task,                           <cef_thread_id_t> -> Ptr <cef_task_t> -> IO <int>
#ccall cef_post_delayed_task,                   <cef_thread_id_t> -> Ptr <cef_task_t> -> <int64> -> IO <int>





#starttype cef_end_tracing_callback_t
#field base,                    <cef_base_t>
#field on_end_tracing_complete, FunPtr (Ptr <cef_end_tracing_callback_t> -> Ptr <cef_string_t> -> IO <void>)
#stoptype

#ccall cef_begin_tracing,               Ptr <cef_string_t> -> IO <int>
#ccall cef_end_tracing_async,           Ptr <cef_string_t> -> Ptr <cef_end_tracing_callback_t> -> IO <int>
#ccall cef_now_from_system_trace_time,  IO <int64>






#starttype cef_urlrequest_t
#field base,                <cef_base_t>
#field get_request,         FunPtr (Ptr <cef_urlrequest_t> -> IO (Ptr <cef_request_t>))
#field get_client,          FunPtr (Ptr <cef_urlrequest_t> -> IO (Ptr <cef_urlrequest_client_t>))
#field get_request_status,  FunPtr (Ptr <cef_urlrequest_t> -> IO <cef_urlrequest_status_t>)
#field get_request_error,   FunPtr (Ptr <cef_urlrequest_t> -> IO <cef_errorcode_t>)
#field get_response,        FunPtr (Ptr <cef_urlrequest_t> -> IO (Ptr <cef_response_t>))
#field cancel,              FunPtr (Ptr <cef_urlrequest_t> -> IO <void>)
#stoptype

#starttype cef_urlrequest_client_t
#field base,                  <cef_base_t>
#field on_request_complete,   FunPtr (Ptr <cef_urlrequest_client_t> -> Ptr <cef_urlrequest_t> -> IO <void>)
#field on_upload_progress,    FunPtr (Ptr <cef_urlrequest_client_t> -> Ptr <cef_urlrequest_t> -> <uint64> -> <uint64> -> IO <void>)
#field on_download_progress,  FunPtr (Ptr <cef_urlrequest_client_t> -> Ptr <cef_urlrequest_t> -> <uint64> -> <uint64> -> IO <void>)
#field on_download_data,      FunPtr (Ptr <cef_urlrequest_client_t> -> Ptr <cef_urlrequest_t> -> Ptr <void> -> <size_t> -> IO <void>)
#field get_auth_credentials,  FunPtr (Ptr <cef_urlrequest_client_t> -> <int> -> Ptr <cef_string_t> -> <int> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> Ptr <cef_auth_callback_t> -> IO <int>)
#stoptype

#ccall cef_urlrequest_create, Ptr <cef_request_t> -> Ptr <cef_urlrequest_client_t> -> IO (Ptr <cef_urlrequest_t>)




#ccall cef_parse_url,     Ptr <cef_string_t> -> Ptr <cef_urlparts_t> -> IO <int>
#ccall cef_create_url,    Ptr <cef_urlparts_t> -> Ptr <cef_string_t> -> IO <int>
#ccall cef_get_mime_type, Ptr <cef_string_t> -> IO <cef_string_userfree_t>






#starttype cef_v8context_t
#field base,                <cef_base_t>
#field get_task_runner,     FunPtr (Ptr <cef_v8context_t> -> IO (Ptr <cef_task_runner_t>))
#field is_valid,            FunPtr (Ptr <cef_v8context_t> -> IO <int>)
#field get_browser,         FunPtr (Ptr <cef_v8context_t> -> IO (Ptr <cef_browser_t>))
#field get_frame,           FunPtr (Ptr <cef_v8context_t> -> IO (Ptr <cef_frame_t>))
#field get_global,          FunPtr (Ptr <cef_v8context_t> -> IO (Ptr <cef_v8value_t>))
#field enter,               FunPtr (Ptr <cef_v8context_t> -> IO <int>)
#field exit,                FunPtr (Ptr <cef_v8context_t> -> IO <int>)
#field is_same,             FunPtr (Ptr <cef_v8context_t> -> Ptr <cef_v8context_t> -> IO <int>)
#field eval,                FunPtr (Ptr <cef_v8context_t> -> Ptr <cef_string_t> -> Ptr (Ptr <cef_v8value_t>) -> Ptr (Ptr <cef_v8exception_t>))
#stoptype

#starttype cef_v8handler_t
#field base,    <cef_base_t>
#field execute, FunPtr (Ptr <cef_v8handler_t> -> Ptr <cef_string_t> -> Ptr <cef_v8value_t> -> <size_t> -> Ptr (Ptr <cef_v8value_t>) -> Ptr (Ptr <cef_v8value_t>) -> Ptr <cef_string_t> -> IO <int>)
#stoptype

#starttype cef_v8accessor_t
#field base,  <cef_base_t>
#field get,   FunPtr (Ptr <cef_v8accessor_t> -> Ptr <cef_string_t> -> Ptr <cef_v8value_t> -> Ptr (Ptr <cef_v8value_t>) -> Ptr <cef_string_t> -> IO <int>)
#field set,   FunPtr (Ptr <cef_v8accessor_t> -> Ptr <cef_string_t> -> Ptr <cef_v8value_t> -> Ptr <cef_v8value_t> -> Ptr <cef_string_t> -> IO <int>)
#stoptype

#starttype cef_v8exception_t
#field base,                      <cef_base_t>
#field get_message,               FunPtr (Ptr <cef_v8exception_t> -> IO <cef_string_userfree_t>)
#field get_source_line,           FunPtr (Ptr <cef_v8exception_t> -> IO <cef_string_userfree_t>)
#field get_script_resource_name,  FunPtr (Ptr <cef_v8exception_t> -> IO <cef_string_userfree_t>)
#field get_line_number,           FunPtr (Ptr <cef_v8exception_t> -> IO <int>)
#field get_start_position,        FunPtr (Ptr <cef_v8exception_t> -> IO <int>)
#field get_end_position,          FunPtr (Ptr <cef_v8exception_t> -> IO <int>)
#field get_start_column,          FunPtr (Ptr <cef_v8exception_t> -> IO <int>)
#field get_end_column,            FunPtr (Ptr <cef_v8exception_t> -> IO <int>)
#stoptype

#starttype cef_v8value_t
#field base,                    <cef_base_t>
#field is_valid,                FunPtr (Ptr <cef_v8value_t> -> IO <int>)
#field is_undefined,            FunPtr (Ptr <cef_v8value_t> -> IO <int>)
#field is_null,                 FunPtr (Ptr <cef_v8value_t> -> IO <int>)
#field is_bool,                 FunPtr (Ptr <cef_v8value_t> -> IO <int>)
#field is_int,                  FunPtr (Ptr <cef_v8value_t> -> IO <int>)
#field is_uint,                 FunPtr (Ptr <cef_v8value_t> -> IO <int>)
#field is_double,               FunPtr (Ptr <cef_v8value_t> -> IO <int>)
#field is_date,                 FunPtr (Ptr <cef_v8value_t> -> IO <int>)
#field is_string,               FunPtr (Ptr <cef_v8value_t> -> IO <int>)
#field is_object,               FunPtr (Ptr <cef_v8value_t> -> IO <int>)
#field is_array,                FunPtr (Ptr <cef_v8value_t> -> IO <int>)
#field is_function,             FunPtr (Ptr <cef_v8value_t> -> IO <int>)
#field is_same,                 FunPtr (Ptr <cef_v8value_t> -> Ptr <cef_v8value_t> -> IO <int>)
#field get_bool_value,          FunPtr (Ptr <cef_v8value_t> -> IO <int>)
#field get_int_value,           FunPtr (Ptr <cef_v8value_t> -> IO <int32>)
#field get_uint_value,          FunPtr (Ptr <cef_v8value_t> -> IO <uint32>)
#field get_double_value,        FunPtr (Ptr <cef_v8value_t> -> IO <double>)
#field get_date_value,          FunPtr (Ptr <cef_v8value_t> -> IO <cef_time_t>)
#field get_string_value,        FunPtr (Ptr <cef_v8value_t> -> IO <cef_string_userfree_t>)
#field is_user_created,         FunPtr (Ptr <cef_v8value_t> -> IO <int>)
#field has_exception,           FunPtr (Ptr <cef_v8value_t> -> IO <int>)
#field get_exception,           FunPtr (Ptr <cef_v8value_t> -> IO (Ptr <cef_v8exception_t>))
#field clear_exception,         FunPtr (Ptr <cef_v8value_t> -> IO <int>)
#field will_rethrow_exceptions, FunPtr (Ptr <cef_v8value_t> -> IO <int>)
#field set_rethrow_exceptions,  FunPtr (Ptr <cef_v8value_t> -> <int> -> IO <int>)
#field has_value_bykey,         FunPtr (Ptr <cef_v8value_t> -> Ptr <cef_string_t> -> IO <int>)
#field has_value_byindex,       FunPtr (Ptr <cef_v8value_t> -> <int> -> IO <int>)
#field delete_value_bykey,      FunPtr (Ptr <cef_v8value_t> -> Ptr <cef_string_t> -> IO <int>)
#field delete_value_byindex,    FunPtr (Ptr <cef_v8value_t> -> <int> -> IO <int>)
#field get_value_bykey,         FunPtr (Ptr <cef_v8value_t> -> Ptr <cef_string_t> -> IO (Ptr <cef_v8value_t>))
#field get_value_byindex,       FunPtr (Ptr <cef_v8value_t> -> <int> -> IO (Ptr <cef_v8value_t>))
#field set_value_bykey,         FunPtr (Ptr <cef_v8value_t> -> Ptr <cef_string_t> -> Ptr <cef_v8value_t> -> <cef_v8_propertyattribute_t> -> IO <int>)
#field set_value_byindex,       FunPtr (Ptr <cef_v8value_t> -> <int> -> Ptr <cef_v8value_t> -> IO <int>)
#field set_value_byaccessor,    FunPtr (Ptr <cef_v8value_t> -> Ptr <cef_string_t> -> <cef_v8_accesscontrol_t> -> <cef_v8_propertyattribute_t> -> IO <int>)
#field get_keys,                FunPtr (Ptr <cef_v8value_t> -> <cef_string_list_t> -> IO <int>)
#field set_user_data,           FunPtr (Ptr <cef_v8value_t> -> Ptr <cef_base_t> -> IO <int>)
#field get_user_data,           FunPtr (Ptr <cef_v8value_t> -> IO (Ptr <cef_base_t>))
#field get_externally_allocated_memory,     FunPtr (Ptr <cef_v8value_t> -> IO <int>)
#field adjust_externally_allocated_memory,  FunPtr (Ptr <cef_v8value_t> -> <int> -> IO <int>)
#field get_array_length,        FunPtr (Ptr <cef_v8value_t> -> IO <int>)
#field get_function_name,       FunPtr (Ptr <cef_v8value_t> -> IO <cef_string_userfree_t>)
#field get_function_handler,    FunPtr (Ptr <cef_v8value_t> -> IO (Ptr <cef_v8handler_t>))
#field execute_function,        FunPtr (Ptr <cef_v8value_t> -> Ptr <cef_v8value_t> -> <size_t> -> Ptr (Ptr <cef_v8value_t>) -> IO (Ptr <cef_v8value_t>))
#field execute_function_with_context,       FunPtr (Ptr <cef_v8value_t> -> Ptr <cef_v8context_t> -> Ptr <cef_v8value_t> -> <size_t> -> Ptr <cef_v8value_t> -> IO (Ptr <cef_v8value_t>))
#stoptype

#starttype cef_v8stack_trace_t
#field base,            <cef_base_t>
#field is_valid,        FunPtr (Ptr <cef_v8stack_trace_t> -> IO <int>)
#field get_frame_count, FunPtr (Ptr <cef_v8stack_trace_t> -> IO <int>)
#field get_frame,       FunPtr (Ptr <cef_v8stack_trace_t> -> <int> -> IO (Ptr <cef_v8stack_frame_t>))
#stoptype

#starttype cef_v8stack_frame_t
#field base,              <cef_base_t>
#field is_valid,          FunPtr (Ptr <cef_v8stack_frame_t> -> IO <int>)
#field get_script_name,   FunPtr (Ptr <cef_v8stack_frame_t> -> IO <cef_string_userfree_t>)
#field get_script_name_or_source_url, FunPtr (Ptr <cef_v8stack_frame_t> -> IO <cef_string_userfree_t>)
#field get_function_name, FunPtr (Ptr <cef_v8stack_frame_t> -> IO <cef_string_userfree_t>)
#field get_line_number,   FunPtr (Ptr <cef_v8stack_frame_t> -> IO <int>)
#field get_column,        FunPtr (Ptr <cef_v8stack_frame_t> -> IO <int>)
#field is_eval,           FunPtr (Ptr <cef_v8stack_frame_t> -> IO <int>)
#field is_constructor,    FunPtr (Ptr <cef_v8stack_frame_t> -> IO <int>)
#stoptype

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
#field is_valid,  FunPtr (Ptr <cef_binary_value_t> -> IO <int>)
#field is_owned,  FunPtr (Ptr <cef_binary_value_t> -> IO <int>)
#field copy,      FunPtr (Ptr <cef_binary_value_t> -> IO (Ptr <cef_binary_value_t>))
#field get_size,  FunPtr (Ptr <cef_binary_value_t> -> IO <size_t>)
#field get_data,  FunPtr (Ptr <cef_binary_value_t> -> Ptr <void> -> <size_t> -> <size_t> -> IO <size_t>)
#stoptype

#starttype cef_dictionary_value_t
#field base,            <cef_base_t>
#field is_valid,        FunPtr (Ptr <cef_dictionary_value_t> -> IO <int>)
#field is_owned,        FunPtr (Ptr <cef_dictionary_value_t> -> IO <int>)
#field is_read_only,    FunPtr (Ptr <cef_dictionary_value_t> -> IO <int>)
#field copy,            FunPtr (Ptr <cef_dictionary_value_t> -> <int> -> IO (Ptr <cef_dictionary_value_t>))
#field get_size,        FunPtr (Ptr <cef_dictionary_value_t> -> IO <size_t>)
#field clear,           FunPtr (Ptr <cef_dictionary_value_t> -> IO <int>)
#field has_key,         FunPtr (Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> IO <int>)
#field get_keys,        FunPtr (Ptr <cef_dictionary_value_t> -> <cef_string_list_t> -> IO <int>)
#field remove,          FunPtr (Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> IO <int>)
#field get_type,        FunPtr (Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> IO <cef_value_type_t>)
#field get_bool,        FunPtr (Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> IO <int>)
#field get_int,         FunPtr (Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> IO <int>)
#field get_double,      FunPtr (Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> IO <double>)
#field get_string,      FunPtr (Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> IO <cef_string_userfree_t>)
#field get_binary,      FunPtr (Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> IO (Ptr <cef_binary_value_t>))
#field get_dictionary,  FunPtr (Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> IO (Ptr <cef_dictionary_value_t>))
#field get_list,        FunPtr (Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> IO (Ptr <cef_list_value_t>))
#field set_null,        FunPtr (Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> IO <int>)
#field set_bool,        FunPtr (Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> <int> -> IO <int>)
#field set_int,         FunPtr (Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> <int> -> IO <int>)
#field set_double,      FunPtr (Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> <double> -> IO <int>)
#field set_string,      FunPtr (Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> IO <int>)
#field set_binary,      FunPtr (Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> Ptr <cef_binary_value_t> -> IO <int>)
#field set_dictionary,  FunPtr (Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> Ptr <cef_dictionary_value_t> -> IO <int>)
#field set_list,        FunPtr (Ptr <cef_dictionary_value_t> -> Ptr <cef_string_t> -> Ptr <cef_list_value_t> -> IO <int>)
#stoptype

#starttype cef_list_value_t
#field base,            <cef_base_t>
#field is_valid,        FunPtr (Ptr <cef_list_value_t> -> IO <int>)
#field is_owned,        FunPtr (Ptr <cef_list_value_t> -> IO <int>)
#field is_read_only,    FunPtr (Ptr <cef_list_value_t> -> IO <int>)
#field copy,            FunPtr (Ptr <cef_list_value_t> -> IO (Ptr <cef_list_value_t>))
#field set_size,        FunPtr (Ptr <cef_list_value_t> -> <size_t> -> IO <int>)
#field get_size,        FunPtr (Ptr <cef_list_value_t> -> IO <size_t>)
#field clear,           FunPtr (Ptr <cef_list_value_t> -> IO <int>)
#field remove,          FunPtr (Ptr <cef_list_value_t> -> <int> -> IO <int>)
#field get_type,        FunPtr (Ptr <cef_list_value_t> -> <int> -> IO <cef_value_type_t>)
#field get_bool,        FunPtr (Ptr <cef_list_value_t> -> <int> -> IO <int>)
#field get_int,         FunPtr (Ptr <cef_list_value_t> -> <int> -> IO <int>)
#field get_double,      FunPtr (Ptr <cef_list_value_t> -> <int> -> IO <double>)
#field get_string,      FunPtr (Ptr <cef_list_value_t> -> <int> -> IO <cef_string_userfree_t>)
#field get_binary,      FunPtr (Ptr <cef_list_value_t> -> <int> -> IO (Ptr <cef_binary_value_t>))
#field get_dictionary,  FunPtr (Ptr <cef_list_value_t> -> <int> -> IO (Ptr <cef_dictionary_value_t>))
#field get_list,        FunPtr (Ptr <cef_list_value_t> -> <int> -> IO (Ptr <cef_list_value_t>))
#field set_null,        FunPtr (Ptr <cef_list_value_t> -> <int> -> IO <int>)
#field set_bool,        FunPtr (Ptr <cef_list_value_t> -> <int> -> <int> -> IO <int>)
#field set_int,         FunPtr (Ptr <cef_list_value_t> -> <int> -> <int> -> IO <int>)
#field set_double,      FunPtr (Ptr <cef_list_value_t> -> <int> -> <double> -> IO <int>)
#field set_string,      FunPtr (Ptr <cef_list_value_t> -> <int> -> Ptr <cef_string_t> -> IO <int>)
#field set_binary,      FunPtr (Ptr <cef_list_value_t> -> <int> -> Ptr <cef_binary_value_t> -> IO <int>)
#field set_dictionary,  FunPtr (Ptr <cef_list_value_t> -> <int> -> Ptr <cef_dictionary_value_t> -> IO <int>)
#field set_list,        FunPtr (Ptr <cef_list_value_t> -> <int> -> Ptr <cef_list_value_t> -> IO <int>)
#stoptype

#ccall cef_binary_value_create, Ptr <void> -> <size_t> -> IO (Ptr <cef_binary_value_t>)
#ccall cef_dictionary_value_create, IO (Ptr <cef_dictionary_value_t>)
#ccall cef_list_value_create, IO (Ptr <cef_list_value_t>)







#starttype cef_web_plugin_info_t
#field base,            <cef_base_t>
#field get_name,        FunPtr (Ptr <cef_web_plugin_info_t> -> IO <cef_string_userfree_t>)
#field get_path,        FunPtr (Ptr <cef_web_plugin_info_t> -> IO <cef_string_userfree_t>)
#field get_version,     FunPtr (Ptr <cef_web_plugin_info_t> -> IO <cef_string_userfree_t>)
#field get_description, FunPtr (Ptr <cef_web_plugin_info_t> -> IO <cef_string_userfree_t>)
#stoptype

#starttype cef_web_plugin_info_visitor_t
#field base,  <cef_base_t>
#field visit, FunPtr (Ptr <cef_web_plugin_info_visitor_t> -> Ptr <cef_web_plugin_info_t> -> <int> -> <int> -> IO <int>)
#stoptype

#starttype cef_web_plugin_unstable_callback_t
#field base,        <cef_base_t>
#field is_unstable, FunPtr (Ptr <cef_web_plugin_unstable_callback_t> -> Ptr <cef_string_t> -> <int> -> IO <void>)
#stoptype

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
#field move_to_next_node,         FunPtr (Ptr <cef_xml_reader_t> -> IO <int>)
#field close,                     FunPtr (Ptr <cef_xml_reader_t> -> IO <int>)
#field has_error,                 FunPtr (Ptr <cef_xml_reader_t> -> IO <int>)
#field get_error,                 FunPtr (Ptr <cef_xml_reader_t> -> IO <cef_string_userfree_t>)
#field get_type,                  FunPtr (Ptr <cef_xml_reader_t> -> IO <cef_xml_node_type_t>)
#field get_depth,                 FunPtr (Ptr <cef_xml_reader_t> -> IO <int>)
#field get_local_name,            FunPtr (Ptr <cef_xml_reader_t> -> IO <cef_string_userfree_t>)
#field get_prefix,                FunPtr (Ptr <cef_xml_reader_t> -> IO <cef_string_userfree_t>)
#field get_qualified_name,        FunPtr (Ptr <cef_xml_reader_t> -> IO <cef_string_userfree_t>)
#field get_namespace_uri,         FunPtr (Ptr <cef_xml_reader_t> -> IO <cef_string_userfree_t>)
#field get_base_uri,              FunPtr (Ptr <cef_xml_reader_t> -> IO <cef_string_userfree_t>)
#field get_xml_lang,              FunPtr (Ptr <cef_xml_reader_t> -> IO <cef_string_userfree_t>)
#field is_empty_element,          FunPtr (Ptr <cef_xml_reader_t> -> IO <int>)
#field has_value,                 FunPtr (Ptr <cef_xml_reader_t> -> IO <int>)
#field get_value,                 FunPtr (Ptr <cef_xml_reader_t> -> IO <cef_string_userfree_t>)
#field has_attributes,            FunPtr (Ptr <cef_xml_reader_t> -> IO <int>)
#field get_attribute_count,       FunPtr (Ptr <cef_xml_reader_t> -> IO <size_t>)
#field get_attribute_byindex,     FunPtr (Ptr <cef_xml_reader_t> -> <int> -> IO <cef_string_userfree_t>)
#field get_attribute_byqname,     FunPtr (Ptr <cef_xml_reader_t> -> Ptr <cef_string_t> -> IO <cef_string_userfree_t>)
#field get_attribute_bylname,     FunPtr (Ptr <cef_xml_reader_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> IO <cef_string_userfree_t>)
#field get_inner_xml,             FunPtr (Ptr <cef_xml_reader_t> -> IO <cef_string_userfree_t>)
#field get_outer_xml,             FunPtr (Ptr <cef_xml_reader_t> -> IO <cef_string_userfree_t>)
#field get_line_number,           FunPtr (Ptr <cef_xml_reader_t> -> IO <int>)
#field move_to_attribute_byindex, FunPtr (Ptr <cef_xml_reader_t> -> <int> -> IO <int>)
#field move_to_attribute_byqname, FunPtr (Ptr <cef_xml_reader_t> -> Ptr <cef_string_t> -> IO <int>)
#field move_to_attribute_bylname, FunPtr (Ptr <cef_xml_reader_t> -> Ptr <cef_string_t> -> Ptr <cef_string_t> -> IO <int>)
#field move_to_first_attribute,   FunPtr (Ptr <cef_xml_reader_t> -> IO <int>)
#field move_to_next_attribute,    FunPtr (Ptr <cef_xml_reader_t> -> IO <int>)
#field move_to_carrying_element,  FunPtr (Ptr <cef_xml_reader_t> -> IO <int>)
#stoptype

#ccall cef_xml_reader_create,     Ptr <cef_stream_reader_t> -> <cef_xml_encoding_type_t> -> Ptr <cef_string_t> -> IO (Ptr <cef_xml_reader_t>)





#starttype cef_zip_reader_t
#field base,                    <cef_base_t>
#field move_to_first_file,      FunPtr (Ptr <cef_zip_reader_t> -> IO <int>)
#field move_to_next_file,       FunPtr (Ptr <cef_zip_reader_t> -> IO <int>)
#field move_to_file,            FunPtr (Ptr <cef_zip_reader_t> -> Ptr <cef_string_t> -> <int> -> IO <int>)
#field close,                   FunPtr (Ptr <cef_zip_reader_t> -> IO <int>)
#field get_file_name,           FunPtr (Ptr <cef_zip_reader_t> -> IO <cef_string_userfree_t>)
#field get_file_size,           FunPtr (Ptr <cef_zip_reader_t> -> IO <int64>)
#field get_file_last_modified,  FunPtr (Ptr <cef_zip_reader_t> -> IO <time_t>)
#field open_file,               FunPtr (Ptr <cef_zip_reader_t> -> Ptr <cef_string_t> -> IO <int>)
#field close_file,              FunPtr (Ptr <cef_zip_reader_t> -> IO <int>)
#field read_file,               FunPtr (Ptr <cef_zip_reader_t> -> Ptr <void> -> <size_t> -> IO <int>)
#field tell,                    FunPtr (Ptr <cef_zip_reader_t> -> IO <int64>)
#field eof,                     FunPtr (Ptr <cef_zip_reader_t> -> IO <int>)
#stoptype

#ccall cef_zip_reader_create,   Ptr <cef_stream_reader_t> -> IO (Ptr <cef_zip_reader_t>)
