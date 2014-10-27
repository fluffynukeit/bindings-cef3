{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include "include/internal/cef_types.h"
#include "include/internal/cef_build.h"
module Bindings.CEF3.Internal.CefTypes 
( module Bindings.CEF3.Internal.CefTypes
, module Bindings.CEF3.Internal.CefTypesLinux
) where
import Foreign.Ptr
#strict_import

import Bindings.CEF3.Internal.CefString
import Bindings.CEF3.Internal.CefStringList
import Bindings.CEF3.Internal.CefTime
import Bindings.CEF3.Internal.CefTypesLinux

{- typedef long int64; -}
#synonym_t int64 , CLong
{- typedef unsigned long uint64; -}
#synonym_t uint64 , CULong
{- typedef int int32; -}
#synonym_t int32 , CInt
{- typedef unsigned int uint32; -}
#synonym_t uint32 , CUInt


{- typedef unsigned short char16; -}
-- #synonym_t char16 , CUShort
-- This is commented out because char16 is also defined in CefStringTypes


{- typedef uint32 cef_color_t; -}
#synonym_t cef_color_t , CUInt
{- typedef enum {
            LOGSEVERITY_DEFAULT,
            LOGSEVERITY_VERBOSE,
            LOGSEVERITY_INFO,
            LOGSEVERITY_WARNING,
            LOGSEVERITY_ERROR,
            LOGSEVERITY_ERROR_REPORT,
            LOGSEVERITY_DISABLE = 99
        } cef_log_severity_t; -}
#integral_t cef_log_severity_t
#num LOGSEVERITY_DEFAULT
#num LOGSEVERITY_VERBOSE
#num LOGSEVERITY_INFO
#num LOGSEVERITY_WARNING
#num LOGSEVERITY_ERROR
#num LOGSEVERITY_ERROR_REPORT
#num LOGSEVERITY_DISABLE
{- typedef enum {
            STATE_DEFAULT = 0, STATE_ENABLED, STATE_DISABLED
        } cef_state_t; -}
#integral_t cef_state_t
#num STATE_DEFAULT
#num STATE_ENABLED
#num STATE_DISABLED
{- typedef cef_settings_t {
            size_t size;
            int single_process;
            int no_sandbox;
            cef_string_t browser_subprocess_path;
            int multi_threaded_message_loop;
            int command_line_args_disabled;
            cef_string_t cache_path;
            int persist_session_cookies;
            cef_string_t user_agent;
            cef_string_t product_version;
            cef_string_t locale;
            cef_string_t log_file;
            cef_log_severity_t log_severity;
            int release_dcheck_enabled;
            cef_string_t javascript_flags;
            cef_string_t resources_dir_path;
            cef_string_t locales_dir_path;
            int pack_loading_disabled;
            int remote_debugging_port;
            int uncaught_exception_stack_size;
            int context_safety_implementation;
            int ignore_certificate_errors;
            cef_color_t background_color;
        } cef_settings_t; -}
#starttype cef_settings_t
#field size , CSize
#field single_process , CInt
#field no_sandbox , CInt
#field browser_subprocess_path , <cef_string_utf16_t>
#field multi_threaded_message_loop , CInt
#field command_line_args_disabled , CInt
#field cache_path , <cef_string_utf16_t>
#field persist_session_cookies , CInt
#field user_agent , <cef_string_utf16_t>
#field product_version , <cef_string_utf16_t>
#field locale , <cef_string_utf16_t>
#field log_file , <cef_string_utf16_t>
#field log_severity , <cef_log_severity_t>
#field release_dcheck_enabled , CInt
#field javascript_flags , <cef_string_utf16_t>
#field resources_dir_path , <cef_string_utf16_t>
#field locales_dir_path , <cef_string_utf16_t>
#field pack_loading_disabled , CInt
#field remote_debugging_port , CInt
#field uncaught_exception_stack_size , CInt
#field context_safety_implementation , CInt
#field ignore_certificate_errors , CInt
#field background_color , CUInt
#stoptype
{- typedef cef_browser_settings_t {
            size_t size;
            cef_string_t standard_font_family;
            cef_string_t fixed_font_family;
            cef_string_t serif_font_family;
            cef_string_t sans_serif_font_family;
            cef_string_t cursive_font_family;
            cef_string_t fantasy_font_family;
            int default_font_size;
            int default_fixed_font_size;
            int minimum_font_size;
            int minimum_logical_font_size;
            cef_string_t default_encoding;
            cef_state_t remote_fonts;
            cef_state_t javascript;
            cef_state_t javascript_open_windows;
            cef_state_t javascript_close_windows;
            cef_state_t javascript_access_clipboard;
            cef_state_t javascript_dom_paste;
            cef_state_t caret_browsing;
            cef_state_t java;
            cef_state_t plugins;
            cef_state_t universal_access_from_file_urls;
            cef_state_t file_access_from_file_urls;
            cef_state_t web_security;
            cef_state_t image_loading;
            cef_state_t image_shrink_standalone_to_fit;
            cef_state_t text_area_resize;
            cef_state_t tab_to_links;
            cef_state_t local_storage;
            cef_state_t databases;
            cef_state_t application_cache;
            cef_state_t webgl;
            cef_state_t accelerated_compositing;
            cef_color_t background_color;
        } cef_browser_settings_t; -}
#starttype cef_browser_settings_t
#field size , CSize
#field standard_font_family , <cef_string_utf16_t>
#field fixed_font_family , <cef_string_utf16_t>
#field serif_font_family , <cef_string_utf16_t>
#field sans_serif_font_family , <cef_string_utf16_t>
#field cursive_font_family , <cef_string_utf16_t>
#field fantasy_font_family , <cef_string_utf16_t>
#field default_font_size , CInt
#field default_fixed_font_size , CInt
#field minimum_font_size , CInt
#field minimum_logical_font_size , CInt
#field default_encoding , <cef_string_utf16_t>
#field remote_fonts , <cef_state_t>
#field javascript , <cef_state_t>
#field javascript_open_windows , <cef_state_t>
#field javascript_close_windows , <cef_state_t>
#field javascript_access_clipboard , <cef_state_t>
#field javascript_dom_paste , <cef_state_t>
#field caret_browsing , <cef_state_t>
#field java , <cef_state_t>
#field plugins , <cef_state_t>
#field universal_access_from_file_urls , <cef_state_t>
#field file_access_from_file_urls , <cef_state_t>
#field web_security , <cef_state_t>
#field image_loading , <cef_state_t>
#field image_shrink_standalone_to_fit , <cef_state_t>
#field text_area_resize , <cef_state_t>
#field tab_to_links , <cef_state_t>
#field local_storage , <cef_state_t>
#field databases , <cef_state_t>
#field application_cache , <cef_state_t>
#field webgl , <cef_state_t>
#field accelerated_compositing , <cef_state_t>
#field background_color , CUInt
#stoptype
{- typedef cef_urlparts_t {
            cef_string_t spec;
            cef_string_t scheme;
            cef_string_t username;
            cef_string_t password;
            cef_string_t host;
            cef_string_t port;
            cef_string_t origin;
            cef_string_t path;
            cef_string_t query;
        } cef_urlparts_t; -}
#starttype cef_urlparts_t
#field spec , <cef_string_utf16_t>
#field scheme , <cef_string_utf16_t>
#field username , <cef_string_utf16_t>
#field password , <cef_string_utf16_t>
#field host , <cef_string_utf16_t>
#field port , <cef_string_utf16_t>
#field origin , <cef_string_utf16_t>
#field path , <cef_string_utf16_t>
#field query , <cef_string_utf16_t>
#stoptype
{- typedef cef_cookie_t {
            cef_string_t name;
            cef_string_t value;
            cef_string_t domain;
            cef_string_t path;
            int secure;
            int httponly;
            cef_time_t creation;
            cef_time_t last_access;
            int has_expires;
            cef_time_t expires;
        } cef_cookie_t; -}
#starttype cef_cookie_t
#field name , <cef_string_utf16_t>
#field value , <cef_string_utf16_t>
#field domain , <cef_string_utf16_t>
#field path , <cef_string_utf16_t>
#field secure , CInt
#field httponly , CInt
#field creation , <cef_time_t>
#field last_access , <cef_time_t>
#field has_expires , CInt
#field expires , <cef_time_t>
#stoptype
{- typedef enum {
            TS_ABNORMAL_TERMINATION, TS_PROCESS_WAS_KILLED, TS_PROCESS_CRASHED
        } cef_termination_status_t; -}
#integral_t cef_termination_status_t
#num TS_ABNORMAL_TERMINATION
#num TS_PROCESS_WAS_KILLED
#num TS_PROCESS_CRASHED
{- typedef enum {
            PK_DIR_CURRENT,
            PK_DIR_EXE,
            PK_DIR_MODULE,
            PK_DIR_TEMP,
            PK_FILE_EXE,
            PK_FILE_MODULE
        } cef_path_key_t; -}
#integral_t cef_path_key_t
#num PK_DIR_CURRENT
#num PK_DIR_EXE
#num PK_DIR_MODULE
#num PK_DIR_TEMP
#num PK_FILE_EXE
#num PK_FILE_MODULE
{- typedef enum {
            ST_LOCALSTORAGE = 0, ST_SESSIONSTORAGE
        } cef_storage_type_t; -}
#integral_t cef_storage_type_t
#num ST_LOCALSTORAGE
#num ST_SESSIONSTORAGE
{- typedef enum {
            ERR_NONE = 0,
            ERR_FAILED = -2,
            ERR_ABORTED = -3,
            ERR_INVALID_ARGUMENT = -4,
            ERR_INVALID_HANDLE = -5,
            ERR_FILE_NOT_FOUND = -6,
            ERR_TIMED_OUT = -7,
            ERR_FILE_TOO_BIG = -8,
            ERR_UNEXPECTED = -9,
            ERR_ACCESS_DENIED = -10,
            ERR_NOT_IMPLEMENTED = -11,
            ERR_CONNECTION_CLOSED = -100,
            ERR_CONNECTION_RESET = -101,
            ERR_CONNECTION_REFUSED = -102,
            ERR_CONNECTION_ABORTED = -103,
            ERR_CONNECTION_FAILED = -104,
            ERR_NAME_NOT_RESOLVED = -105,
            ERR_INTERNET_DISCONNECTED = -106,
            ERR_SSL_PROTOCOL_ERROR = -107,
            ERR_ADDRESS_INVALID = -108,
            ERR_ADDRESS_UNREACHABLE = -109,
            ERR_SSL_CLIENT_AUTH_CERT_NEEDED = -110,
            ERR_TUNNEL_CONNECTION_FAILED = -111,
            ERR_NO_SSL_VERSIONS_ENABLED = -112,
            ERR_SSL_VERSION_OR_CIPHER_MISMATCH = -113,
            ERR_SSL_RENEGOTIATION_REQUESTED = -114,
            ERR_CERT_COMMON_NAME_INVALID = -200,
            ERR_CERT_DATE_INVALID = -201,
            ERR_CERT_AUTHORITY_INVALID = -202,
            ERR_CERT_CONTAINS_ERRORS = -203,
            ERR_CERT_NO_REVOCATION_MECHANISM = -204,
            ERR_CERT_UNABLE_TO_CHECK_REVOCATION = -205,
            ERR_CERT_REVOKED = -206,
            ERR_CERT_INVALID = -207,
            ERR_CERT_END = -208,
            ERR_INVALID_URL = -300,
            ERR_DISALLOWED_URL_SCHEME = -301,
            ERR_UNKNOWN_URL_SCHEME = -302,
            ERR_TOO_MANY_REDIRECTS = -310,
            ERR_UNSAFE_REDIRECT = -311,
            ERR_UNSAFE_PORT = -312,
            ERR_INVALID_RESPONSE = -320,
            ERR_INVALID_CHUNKED_ENCODING = -321,
            ERR_METHOD_NOT_SUPPORTED = -322,
            ERR_UNEXPECTED_PROXY_AUTH = -323,
            ERR_EMPTY_RESPONSE = -324,
            ERR_RESPONSE_HEADERS_TOO_BIG = -325,
            ERR_CACHE_MISS = -400,
            ERR_INSECURE_RESPONSE = -501
        } cef_errorcode_t; -}
#integral_t cef_errorcode_t
#num ERR_NONE
#num ERR_FAILED
#num ERR_ABORTED
#num ERR_INVALID_ARGUMENT
#num ERR_INVALID_HANDLE
#num ERR_FILE_NOT_FOUND
#num ERR_TIMED_OUT
#num ERR_FILE_TOO_BIG
#num ERR_UNEXPECTED
#num ERR_ACCESS_DENIED
#num ERR_NOT_IMPLEMENTED
#num ERR_CONNECTION_CLOSED
#num ERR_CONNECTION_RESET
#num ERR_CONNECTION_REFUSED
#num ERR_CONNECTION_ABORTED
#num ERR_CONNECTION_FAILED
#num ERR_NAME_NOT_RESOLVED
#num ERR_INTERNET_DISCONNECTED
#num ERR_SSL_PROTOCOL_ERROR
#num ERR_ADDRESS_INVALID
#num ERR_ADDRESS_UNREACHABLE
#num ERR_SSL_CLIENT_AUTH_CERT_NEEDED
#num ERR_TUNNEL_CONNECTION_FAILED
#num ERR_NO_SSL_VERSIONS_ENABLED
#num ERR_SSL_VERSION_OR_CIPHER_MISMATCH
#num ERR_SSL_RENEGOTIATION_REQUESTED
#num ERR_CERT_COMMON_NAME_INVALID
#num ERR_CERT_DATE_INVALID
#num ERR_CERT_AUTHORITY_INVALID
#num ERR_CERT_CONTAINS_ERRORS
#num ERR_CERT_NO_REVOCATION_MECHANISM
#num ERR_CERT_UNABLE_TO_CHECK_REVOCATION
#num ERR_CERT_REVOKED
#num ERR_CERT_INVALID
#num ERR_CERT_END
#num ERR_INVALID_URL
#num ERR_DISALLOWED_URL_SCHEME
#num ERR_UNKNOWN_URL_SCHEME
#num ERR_TOO_MANY_REDIRECTS
#num ERR_UNSAFE_REDIRECT
#num ERR_UNSAFE_PORT
#num ERR_INVALID_RESPONSE
#num ERR_INVALID_CHUNKED_ENCODING
#num ERR_METHOD_NOT_SUPPORTED
#num ERR_UNEXPECTED_PROXY_AUTH
#num ERR_EMPTY_RESPONSE
#num ERR_RESPONSE_HEADERS_TOO_BIG
#num ERR_CACHE_MISS
#num ERR_INSECURE_RESPONSE
{- typedef enum {
            DRAG_OPERATION_NONE = 0,
            DRAG_OPERATION_COPY = 1,
            DRAG_OPERATION_LINK = 2,
            DRAG_OPERATION_GENERIC = 4,
            DRAG_OPERATION_PRIVATE = 8,
            DRAG_OPERATION_MOVE = 16,
            DRAG_OPERATION_DELETE = 32,
            DRAG_OPERATION_EVERY = 2147483647 * 2u + 1u
        } cef_drag_operations_mask_t; -}
#integral_t cef_drag_operations_mask_t
#num DRAG_OPERATION_NONE
#num DRAG_OPERATION_COPY
#num DRAG_OPERATION_LINK
#num DRAG_OPERATION_GENERIC
#num DRAG_OPERATION_PRIVATE
#num DRAG_OPERATION_MOVE
#num DRAG_OPERATION_DELETE
#num DRAG_OPERATION_EVERY
{- typedef enum {
            V8_ACCESS_CONTROL_DEFAULT = 0,
            V8_ACCESS_CONTROL_ALL_CAN_READ = 1,
            V8_ACCESS_CONTROL_ALL_CAN_WRITE = 1 << 1,
            V8_ACCESS_CONTROL_PROHIBITS_OVERWRITING = 1 << 2
        } cef_v8_accesscontrol_t; -}
#integral_t cef_v8_accesscontrol_t
#num V8_ACCESS_CONTROL_DEFAULT
#num V8_ACCESS_CONTROL_ALL_CAN_READ
#num V8_ACCESS_CONTROL_ALL_CAN_WRITE
#num V8_ACCESS_CONTROL_PROHIBITS_OVERWRITING
{- typedef enum {
            V8_PROPERTY_ATTRIBUTE_NONE = 0,
            V8_PROPERTY_ATTRIBUTE_READONLY = 1 << 0,
            V8_PROPERTY_ATTRIBUTE_DONTENUM = 1 << 1,
            V8_PROPERTY_ATTRIBUTE_DONTDELETE = 1 << 2
        } cef_v8_propertyattribute_t; -}
#integral_t cef_v8_propertyattribute_t
#num V8_PROPERTY_ATTRIBUTE_NONE
#num V8_PROPERTY_ATTRIBUTE_READONLY
#num V8_PROPERTY_ATTRIBUTE_DONTENUM
#num V8_PROPERTY_ATTRIBUTE_DONTDELETE
{- typedef enum {
            PDE_TYPE_EMPTY = 0, PDE_TYPE_BYTES, PDE_TYPE_FILE
        } cef_postdataelement_type_t; -}
#integral_t cef_postdataelement_type_t
#num PDE_TYPE_EMPTY
#num PDE_TYPE_BYTES
#num PDE_TYPE_FILE
{- typedef enum {
            RT_MAIN_FRAME = 0,
            RT_SUB_FRAME,
            RT_STYLESHEET,
            RT_SCRIPT,
            RT_IMAGE,
            RT_FONT_RESOURCE,
            RT_SUB_RESOURCE,
            RT_OBJECT,
            RT_MEDIA,
            RT_WORKER,
            RT_SHARED_WORKER,
            RT_PREFETCH,
            RT_FAVICON,
            RT_XHR
        } cef_resource_type_t; -}
#integral_t cef_resource_type_t
#num RT_MAIN_FRAME
#num RT_SUB_FRAME
#num RT_STYLESHEET
#num RT_SCRIPT
#num RT_IMAGE
#num RT_FONT_RESOURCE
#num RT_SUB_RESOURCE
#num RT_OBJECT
#num RT_MEDIA
#num RT_WORKER
#num RT_SHARED_WORKER
#num RT_PREFETCH
#num RT_FAVICON
#num RT_XHR
{- typedef enum {
            TT_LINK = 0,
            TT_EXPLICIT = 1,
            TT_AUTO_SUBFRAME = 3,
            TT_MANUAL_SUBFRAME = 4,
            TT_FORM_SUBMIT = 7,
            TT_RELOAD = 8,
            TT_SOURCE_MASK = 0xff,
            TT_BLOCKED_FLAG = 0x800000,
            TT_FORWARD_BACK_FLAG = 0x1000000,
            TT_CHAIN_START_FLAG = 0x10000000,
            TT_CHAIN_END_FLAG = 0x20000000,
            TT_CLIENT_REDIRECT_FLAG = 0x40000000,
            TT_SERVER_REDIRECT_FLAG = 0x80000000,
            TT_IS_REDIRECT_MASK = 0xc0000000,
            TT_QUALIFIER_MASK = 0xffffff00
        } cef_transition_type_t; -}
#integral_t cef_transition_type_t
#num TT_LINK
#num TT_EXPLICIT
#num TT_AUTO_SUBFRAME
#num TT_MANUAL_SUBFRAME
#num TT_FORM_SUBMIT
#num TT_RELOAD
#num TT_SOURCE_MASK
#num TT_BLOCKED_FLAG
#num TT_FORWARD_BACK_FLAG
#num TT_CHAIN_START_FLAG
#num TT_CHAIN_END_FLAG
#num TT_CLIENT_REDIRECT_FLAG
#num TT_SERVER_REDIRECT_FLAG
#num TT_IS_REDIRECT_MASK
#num TT_QUALIFIER_MASK
{- typedef enum {
            UR_FLAG_NONE = 0,
            UR_FLAG_SKIP_CACHE = 1 << 0,
            UR_FLAG_ALLOW_CACHED_CREDENTIALS = 1 << 1,
            UR_FLAG_ALLOW_COOKIES = 1 << 2,
            UR_FLAG_REPORT_UPLOAD_PROGRESS = 1 << 3,
            UR_FLAG_REPORT_LOAD_TIMING = 1 << 4,
            UR_FLAG_REPORT_RAW_HEADERS = 1 << 5,
            UR_FLAG_NO_DOWNLOAD_DATA = 1 << 6,
            UR_FLAG_NO_RETRY_ON_5XX = 1 << 7
        } cef_urlrequest_flags_t; -}
#integral_t cef_urlrequest_flags_t
#num UR_FLAG_NONE
#num UR_FLAG_SKIP_CACHE
#num UR_FLAG_ALLOW_CACHED_CREDENTIALS
#num UR_FLAG_ALLOW_COOKIES
#num UR_FLAG_REPORT_UPLOAD_PROGRESS
#num UR_FLAG_REPORT_LOAD_TIMING
#num UR_FLAG_REPORT_RAW_HEADERS
#num UR_FLAG_NO_DOWNLOAD_DATA
#num UR_FLAG_NO_RETRY_ON_5XX
{- typedef enum {
            UR_UNKNOWN = 0, UR_SUCCESS, UR_IO_PENDING, UR_CANCELED, UR_FAILED
        } cef_urlrequest_status_t; -}
#integral_t cef_urlrequest_status_t
#num UR_UNKNOWN
#num UR_SUCCESS
#num UR_IO_PENDING
#num UR_CANCELED
#num UR_FAILED
{- typedef cef_rect_t {
            int x; int y; int width; int height;
        } cef_rect_t; -}
#starttype cef_rect_t
#field x , CInt
#field y , CInt
#field width , CInt
#field height , CInt
#stoptype
{- typedef enum {
            PID_BROWSER, PID_RENDERER
        } cef_process_id_t; -}
#integral_t cef_process_id_t
#num PID_BROWSER
#num PID_RENDERER
{- typedef enum {
            TID_UI,
            TID_DB,
            TID_FILE,
            TID_FILE_USER_BLOCKING,
            TID_PROCESS_LAUNCHER,
            TID_CACHE,
            TID_IO,
            TID_RENDERER
        } cef_thread_id_t; -}
#integral_t cef_thread_id_t
#num TID_UI
#num TID_DB
#num TID_FILE
#num TID_FILE_USER_BLOCKING
#num TID_PROCESS_LAUNCHER
#num TID_CACHE
#num TID_IO
#num TID_RENDERER
{- typedef enum {
            VTYPE_INVALID = 0,
            VTYPE_NULL,
            VTYPE_BOOL,
            VTYPE_INT,
            VTYPE_DOUBLE,
            VTYPE_STRING,
            VTYPE_BINARY,
            VTYPE_DICTIONARY,
            VTYPE_LIST
        } cef_value_type_t; -}
#integral_t cef_value_type_t
#num VTYPE_INVALID
#num VTYPE_NULL
#num VTYPE_BOOL
#num VTYPE_INT
#num VTYPE_DOUBLE
#num VTYPE_STRING
#num VTYPE_BINARY
#num VTYPE_DICTIONARY
#num VTYPE_LIST
{- typedef enum {
            JSDIALOGTYPE_ALERT = 0, JSDIALOGTYPE_CONFIRM, JSDIALOGTYPE_PROMPT
        } cef_jsdialog_type_t; -}
#integral_t cef_jsdialog_type_t
#num JSDIALOGTYPE_ALERT
#num JSDIALOGTYPE_CONFIRM
#num JSDIALOGTYPE_PROMPT
{- typedef cef_screen_info_t {
            float device_scale_factor;
            int depth;
            int depth_per_component;
            int is_monochrome;
            cef_rect_t rect;
            cef_rect_t available_rect;
        } cef_screen_info_t; -}
#starttype cef_screen_info_t
#field device_scale_factor , CFloat
#field depth , CInt
#field depth_per_component , CInt
#field is_monochrome , CInt
#field rect , <cef_rect_t>
#field available_rect , <cef_rect_t>
#stoptype
{- typedef enum {
            MENU_ID_BACK = 100,
            MENU_ID_FORWARD = 101,
            MENU_ID_RELOAD = 102,
            MENU_ID_RELOAD_NOCACHE = 103,
            MENU_ID_STOPLOAD = 104,
            MENU_ID_UNDO = 110,
            MENU_ID_REDO = 111,
            MENU_ID_CUT = 112,
            MENU_ID_COPY = 113,
            MENU_ID_PASTE = 114,
            MENU_ID_DELETE = 115,
            MENU_ID_SELECT_ALL = 116,
            MENU_ID_FIND = 130,
            MENU_ID_PRINT = 131,
            MENU_ID_VIEW_SOURCE = 132,
            MENU_ID_USER_FIRST = 26500,
            MENU_ID_USER_LAST = 28500
        } cef_menu_id_t; -}
#integral_t cef_menu_id_t
#num MENU_ID_BACK
#num MENU_ID_FORWARD
#num MENU_ID_RELOAD
#num MENU_ID_RELOAD_NOCACHE
#num MENU_ID_STOPLOAD
#num MENU_ID_UNDO
#num MENU_ID_REDO
#num MENU_ID_CUT
#num MENU_ID_COPY
#num MENU_ID_PASTE
#num MENU_ID_DELETE
#num MENU_ID_SELECT_ALL
#num MENU_ID_FIND
#num MENU_ID_PRINT
#num MENU_ID_VIEW_SOURCE
#num MENU_ID_USER_FIRST
#num MENU_ID_USER_LAST
{- typedef enum {
            MBT_LEFT = 0, MBT_MIDDLE, MBT_RIGHT
        } cef_mouse_button_type_t; -}
#integral_t cef_mouse_button_type_t
#num MBT_LEFT
#num MBT_MIDDLE
#num MBT_RIGHT
{- typedef cef_mouse_event_t {
            int x; int y; uint32 modifiers;
        } cef_mouse_event_t; -}
#starttype cef_mouse_event_t
#field x , CInt
#field y , CInt
#field modifiers , CUInt
#stoptype
{- typedef enum {
            PET_VIEW = 0, PET_POPUP
        } cef_paint_element_type_t; -}
#integral_t cef_paint_element_type_t
#num PET_VIEW
#num PET_POPUP
{- typedef enum {
            EVENTFLAG_NONE = 0,
            EVENTFLAG_CAPS_LOCK_ON = 1 << 0,
            EVENTFLAG_SHIFT_DOWN = 1 << 1,
            EVENTFLAG_CONTROL_DOWN = 1 << 2,
            EVENTFLAG_ALT_DOWN = 1 << 3,
            EVENTFLAG_LEFT_MOUSE_BUTTON = 1 << 4,
            EVENTFLAG_MIDDLE_MOUSE_BUTTON = 1 << 5,
            EVENTFLAG_RIGHT_MOUSE_BUTTON = 1 << 6,
            EVENTFLAG_COMMAND_DOWN = 1 << 7,
            EVENTFLAG_NUM_LOCK_ON = 1 << 8,
            EVENTFLAG_IS_KEY_PAD = 1 << 9,
            EVENTFLAG_IS_LEFT = 1 << 10,
            EVENTFLAG_IS_RIGHT = 1 << 11
        } cef_event_flags_t; -}
#integral_t cef_event_flags_t
#num EVENTFLAG_NONE
#num EVENTFLAG_CAPS_LOCK_ON
#num EVENTFLAG_SHIFT_DOWN
#num EVENTFLAG_CONTROL_DOWN
#num EVENTFLAG_ALT_DOWN
#num EVENTFLAG_LEFT_MOUSE_BUTTON
#num EVENTFLAG_MIDDLE_MOUSE_BUTTON
#num EVENTFLAG_RIGHT_MOUSE_BUTTON
#num EVENTFLAG_COMMAND_DOWN
#num EVENTFLAG_NUM_LOCK_ON
#num EVENTFLAG_IS_KEY_PAD
#num EVENTFLAG_IS_LEFT
#num EVENTFLAG_IS_RIGHT
{- typedef enum {
            MENUITEMTYPE_NONE,
            MENUITEMTYPE_COMMAND,
            MENUITEMTYPE_CHECK,
            MENUITEMTYPE_RADIO,
            MENUITEMTYPE_SEPARATOR,
            MENUITEMTYPE_SUBMENU
        } cef_menu_item_type_t; -}
#integral_t cef_menu_item_type_t
#num MENUITEMTYPE_NONE
#num MENUITEMTYPE_COMMAND
#num MENUITEMTYPE_CHECK
#num MENUITEMTYPE_RADIO
#num MENUITEMTYPE_SEPARATOR
#num MENUITEMTYPE_SUBMENU
{- typedef enum {
            CM_TYPEFLAG_NONE = 0,
            CM_TYPEFLAG_PAGE = 1 << 0,
            CM_TYPEFLAG_FRAME = 1 << 1,
            CM_TYPEFLAG_LINK = 1 << 2,
            CM_TYPEFLAG_MEDIA = 1 << 3,
            CM_TYPEFLAG_SELECTION = 1 << 4,
            CM_TYPEFLAG_EDITABLE = 1 << 5
        } cef_context_menu_type_flags_t; -}
#integral_t cef_context_menu_type_flags_t
#num CM_TYPEFLAG_NONE
#num CM_TYPEFLAG_PAGE
#num CM_TYPEFLAG_FRAME
#num CM_TYPEFLAG_LINK
#num CM_TYPEFLAG_MEDIA
#num CM_TYPEFLAG_SELECTION
#num CM_TYPEFLAG_EDITABLE
{- typedef enum {
            CM_MEDIATYPE_NONE,
            CM_MEDIATYPE_IMAGE,
            CM_MEDIATYPE_VIDEO,
            CM_MEDIATYPE_AUDIO,
            CM_MEDIATYPE_FILE,
            CM_MEDIATYPE_PLUGIN
        } cef_context_menu_media_type_t; -}
#integral_t cef_context_menu_media_type_t
#num CM_MEDIATYPE_NONE
#num CM_MEDIATYPE_IMAGE
#num CM_MEDIATYPE_VIDEO
#num CM_MEDIATYPE_AUDIO
#num CM_MEDIATYPE_FILE
#num CM_MEDIATYPE_PLUGIN
{- typedef enum {
            CM_MEDIAFLAG_NONE = 0,
            CM_MEDIAFLAG_ERROR = 1 << 0,
            CM_MEDIAFLAG_PAUSED = 1 << 1,
            CM_MEDIAFLAG_MUTED = 1 << 2,
            CM_MEDIAFLAG_LOOP = 1 << 3,
            CM_MEDIAFLAG_CAN_SAVE = 1 << 4,
            CM_MEDIAFLAG_HAS_AUDIO = 1 << 5,
            CM_MEDIAFLAG_HAS_VIDEO = 1 << 6,
            CM_MEDIAFLAG_CONTROL_ROOT_ELEMENT = 1 << 7,
            CM_MEDIAFLAG_CAN_PRINT = 1 << 8,
            CM_MEDIAFLAG_CAN_ROTATE = 1 << 9
        } cef_context_menu_media_state_flags_t; -}
#integral_t cef_context_menu_media_state_flags_t
#num CM_MEDIAFLAG_NONE
#num CM_MEDIAFLAG_ERROR
#num CM_MEDIAFLAG_PAUSED
#num CM_MEDIAFLAG_MUTED
#num CM_MEDIAFLAG_LOOP
#num CM_MEDIAFLAG_CAN_SAVE
#num CM_MEDIAFLAG_HAS_AUDIO
#num CM_MEDIAFLAG_HAS_VIDEO
#num CM_MEDIAFLAG_CONTROL_ROOT_ELEMENT
#num CM_MEDIAFLAG_CAN_PRINT
#num CM_MEDIAFLAG_CAN_ROTATE
{- typedef enum {
            CM_EDITFLAG_NONE = 0,
            CM_EDITFLAG_CAN_UNDO = 1 << 0,
            CM_EDITFLAG_CAN_REDO = 1 << 1,
            CM_EDITFLAG_CAN_CUT = 1 << 2,
            CM_EDITFLAG_CAN_COPY = 1 << 3,
            CM_EDITFLAG_CAN_PASTE = 1 << 4,
            CM_EDITFLAG_CAN_DELETE = 1 << 5,
            CM_EDITFLAG_CAN_SELECT_ALL = 1 << 6,
            CM_EDITFLAG_CAN_TRANSLATE = 1 << 7
        } cef_context_menu_edit_state_flags_t; -}
#integral_t cef_context_menu_edit_state_flags_t
#num CM_EDITFLAG_NONE
#num CM_EDITFLAG_CAN_UNDO
#num CM_EDITFLAG_CAN_REDO
#num CM_EDITFLAG_CAN_CUT
#num CM_EDITFLAG_CAN_COPY
#num CM_EDITFLAG_CAN_PASTE
#num CM_EDITFLAG_CAN_DELETE
#num CM_EDITFLAG_CAN_SELECT_ALL
#num CM_EDITFLAG_CAN_TRANSLATE
{- typedef enum {
            KEYEVENT_RAWKEYDOWN = 0,
            KEYEVENT_KEYDOWN,
            KEYEVENT_KEYUP,
            KEYEVENT_CHAR
        } cef_key_event_type_t; -}
#integral_t cef_key_event_type_t
#num KEYEVENT_RAWKEYDOWN
#num KEYEVENT_KEYDOWN
#num KEYEVENT_KEYUP
#num KEYEVENT_CHAR
{- typedef cef_key_event_t {
            cef_key_event_type_t type;
            uint32 modifiers;
            int windows_key_code;
            int native_key_code;
            int is_system_key;
            char16 character;
            char16 unmodified_character;
            int focus_on_editable_field;
        } cef_key_event_t; -}
#starttype cef_key_event_t
#field type , <cef_key_event_type_t>
#field modifiers , CUInt
#field windows_key_code , CInt
#field native_key_code , CInt
#field is_system_key , CInt
#field character , CUShort
#field unmodified_character , CUShort
#field focus_on_editable_field , CInt
#stoptype
{- typedef enum {
            FOCUS_SOURCE_NAVIGATION = 0, FOCUS_SOURCE_SYSTEM
        } cef_focus_source_t; -}
#integral_t cef_focus_source_t
#num FOCUS_SOURCE_NAVIGATION
#num FOCUS_SOURCE_SYSTEM
{- typedef enum {
            NAVIGATION_LINK_CLICKED = 0,
            NAVIGATION_FORM_SUBMITTED,
            NAVIGATION_BACK_FORWARD,
            NAVIGATION_RELOAD,
            NAVIGATION_FORM_RESUBMITTED,
            NAVIGATION_OTHER
        } cef_navigation_type_t; -}
#integral_t cef_navigation_type_t
#num NAVIGATION_LINK_CLICKED
#num NAVIGATION_FORM_SUBMITTED
#num NAVIGATION_BACK_FORWARD
#num NAVIGATION_RELOAD
#num NAVIGATION_FORM_RESUBMITTED
#num NAVIGATION_OTHER
{- typedef enum {
            XML_ENCODING_NONE = 0,
            XML_ENCODING_UTF8,
            XML_ENCODING_UTF16LE,
            XML_ENCODING_UTF16BE,
            XML_ENCODING_ASCII
        } cef_xml_encoding_type_t; -}
#integral_t cef_xml_encoding_type_t
#num XML_ENCODING_NONE
#num XML_ENCODING_UTF8
#num XML_ENCODING_UTF16LE
#num XML_ENCODING_UTF16BE
#num XML_ENCODING_ASCII
{- typedef enum {
            XML_NODE_UNSUPPORTED = 0,
            XML_NODE_PROCESSING_INSTRUCTION,
            XML_NODE_DOCUMENT_TYPE,
            XML_NODE_ELEMENT_START,
            XML_NODE_ELEMENT_END,
            XML_NODE_ATTRIBUTE,
            XML_NODE_TEXT,
            XML_NODE_CDATA,
            XML_NODE_ENTITY_REFERENCE,
            XML_NODE_WHITESPACE,
            XML_NODE_COMMENT
        } cef_xml_node_type_t; -}
#integral_t cef_xml_node_type_t
#num XML_NODE_UNSUPPORTED
#num XML_NODE_PROCESSING_INSTRUCTION
#num XML_NODE_DOCUMENT_TYPE
#num XML_NODE_ELEMENT_START
#num XML_NODE_ELEMENT_END
#num XML_NODE_ATTRIBUTE
#num XML_NODE_TEXT
#num XML_NODE_CDATA
#num XML_NODE_ENTITY_REFERENCE
#num XML_NODE_WHITESPACE
#num XML_NODE_COMMENT
{- typedef cef_popup_features_t {
            int x;
            int xSet;
            int y;
            int ySet;
            int width;
            int widthSet;
            int height;
            int heightSet;
            int menuBarVisible;
            int statusBarVisible;
            int toolBarVisible;
            int locationBarVisible;
            int scrollbarsVisible;
            int resizable;
            int fullscreen;
            int dialog;
            cef_string_list_t additionalFeatures;
        } cef_popup_features_t; -}
#starttype cef_popup_features_t
#field x , CInt
#field xSet , CInt
#field y , CInt
#field ySet , CInt
#field width , CInt
#field widthSet , CInt
#field height , CInt
#field heightSet , CInt
#field menuBarVisible , CInt
#field statusBarVisible , CInt
#field toolBarVisible , CInt
#field locationBarVisible , CInt
#field scrollbarsVisible , CInt
#field resizable , CInt
#field fullscreen , CInt
#field dialog , CInt
#field additionalFeatures , <cef_string_list_t>
#stoptype
{- typedef enum {
            DOM_DOCUMENT_TYPE_UNKNOWN = 0,
            DOM_DOCUMENT_TYPE_HTML,
            DOM_DOCUMENT_TYPE_XHTML,
            DOM_DOCUMENT_TYPE_PLUGIN
        } cef_dom_document_type_t; -}
#integral_t cef_dom_document_type_t
#num DOM_DOCUMENT_TYPE_UNKNOWN
#num DOM_DOCUMENT_TYPE_HTML
#num DOM_DOCUMENT_TYPE_XHTML
#num DOM_DOCUMENT_TYPE_PLUGIN
{- typedef enum {
            DOM_EVENT_CATEGORY_UNKNOWN = 0x0,
            DOM_EVENT_CATEGORY_UI = 0x1,
            DOM_EVENT_CATEGORY_MOUSE = 0x2,
            DOM_EVENT_CATEGORY_MUTATION = 0x4,
            DOM_EVENT_CATEGORY_KEYBOARD = 0x8,
            DOM_EVENT_CATEGORY_TEXT = 0x10,
            DOM_EVENT_CATEGORY_COMPOSITION = 0x20,
            DOM_EVENT_CATEGORY_DRAG = 0x40,
            DOM_EVENT_CATEGORY_CLIPBOARD = 0x80,
            DOM_EVENT_CATEGORY_MESSAGE = 0x100,
            DOM_EVENT_CATEGORY_WHEEL = 0x200,
            DOM_EVENT_CATEGORY_BEFORE_TEXT_INSERTED = 0x400,
            DOM_EVENT_CATEGORY_OVERFLOW = 0x800,
            DOM_EVENT_CATEGORY_PAGE_TRANSITION = 0x1000,
            DOM_EVENT_CATEGORY_POPSTATE = 0x2000,
            DOM_EVENT_CATEGORY_PROGRESS = 0x4000,
            DOM_EVENT_CATEGORY_XMLHTTPREQUEST_PROGRESS = 0x8000,
            DOM_EVENT_CATEGORY_BEFORE_LOAD = 0x10000
        } cef_dom_event_category_t; -}
#integral_t cef_dom_event_category_t
#num DOM_EVENT_CATEGORY_UNKNOWN
#num DOM_EVENT_CATEGORY_UI
#num DOM_EVENT_CATEGORY_MOUSE
#num DOM_EVENT_CATEGORY_MUTATION
#num DOM_EVENT_CATEGORY_KEYBOARD
#num DOM_EVENT_CATEGORY_TEXT
#num DOM_EVENT_CATEGORY_COMPOSITION
#num DOM_EVENT_CATEGORY_DRAG
#num DOM_EVENT_CATEGORY_CLIPBOARD
#num DOM_EVENT_CATEGORY_MESSAGE
#num DOM_EVENT_CATEGORY_WHEEL
#num DOM_EVENT_CATEGORY_BEFORE_TEXT_INSERTED
#num DOM_EVENT_CATEGORY_OVERFLOW
#num DOM_EVENT_CATEGORY_PAGE_TRANSITION
#num DOM_EVENT_CATEGORY_POPSTATE
#num DOM_EVENT_CATEGORY_PROGRESS
#num DOM_EVENT_CATEGORY_XMLHTTPREQUEST_PROGRESS
#num DOM_EVENT_CATEGORY_BEFORE_LOAD
{- typedef enum {
            DOM_EVENT_PHASE_UNKNOWN = 0,
            DOM_EVENT_PHASE_CAPTURING,
            DOM_EVENT_PHASE_AT_TARGET,
            DOM_EVENT_PHASE_BUBBLING
        } cef_dom_event_phase_t; -}
#integral_t cef_dom_event_phase_t
#num DOM_EVENT_PHASE_UNKNOWN
#num DOM_EVENT_PHASE_CAPTURING
#num DOM_EVENT_PHASE_AT_TARGET
#num DOM_EVENT_PHASE_BUBBLING
{- typedef enum {
            DOM_NODE_TYPE_UNSUPPORTED = 0,
            DOM_NODE_TYPE_ELEMENT,
            DOM_NODE_TYPE_ATTRIBUTE,
            DOM_NODE_TYPE_TEXT,
            DOM_NODE_TYPE_CDATA_SECTION,
            DOM_NODE_TYPE_ENTITY,
            DOM_NODE_TYPE_PROCESSING_INSTRUCTIONS,
            DOM_NODE_TYPE_COMMENT,
            DOM_NODE_TYPE_DOCUMENT,
            DOM_NODE_TYPE_DOCUMENT_TYPE,
            DOM_NODE_TYPE_DOCUMENT_FRAGMENT,
            DOM_NODE_TYPE_NOTATION,
            DOM_NODE_TYPE_XPATH_NAMESPACE
        } cef_dom_node_type_t; -}
#integral_t cef_dom_node_type_t
#num DOM_NODE_TYPE_UNSUPPORTED
#num DOM_NODE_TYPE_ELEMENT
#num DOM_NODE_TYPE_ATTRIBUTE
#num DOM_NODE_TYPE_TEXT
#num DOM_NODE_TYPE_CDATA_SECTION
#num DOM_NODE_TYPE_ENTITY
#num DOM_NODE_TYPE_PROCESSING_INSTRUCTIONS
#num DOM_NODE_TYPE_COMMENT
#num DOM_NODE_TYPE_DOCUMENT
#num DOM_NODE_TYPE_DOCUMENT_TYPE
#num DOM_NODE_TYPE_DOCUMENT_FRAGMENT
#num DOM_NODE_TYPE_NOTATION
#num DOM_NODE_TYPE_XPATH_NAMESPACE
{- typedef enum {
            FILE_DIALOG_OPEN = 0, FILE_DIALOG_OPEN_MULTIPLE, FILE_DIALOG_SAVE
        } cef_file_dialog_mode_t; -}
#integral_t cef_file_dialog_mode_t
#num FILE_DIALOG_OPEN
#num FILE_DIALOG_OPEN_MULTIPLE
#num FILE_DIALOG_SAVE
{- typedef enum {
            GEOPOSITON_ERROR_NONE = 0,
            GEOPOSITON_ERROR_PERMISSION_DENIED,
            GEOPOSITON_ERROR_POSITION_UNAVAILABLE,
            GEOPOSITON_ERROR_TIMEOUT
        } cef_geoposition_error_code_t; -}
#integral_t cef_geoposition_error_code_t
#num GEOPOSITON_ERROR_NONE
#num GEOPOSITON_ERROR_PERMISSION_DENIED
#num GEOPOSITON_ERROR_POSITION_UNAVAILABLE
#num GEOPOSITON_ERROR_TIMEOUT
{- typedef cef_geoposition_t {
            double latitude;
            double longitude;
            double altitude;
            double accuracy;
            double altitude_accuracy;
            double heading;
            double speed;
            cef_time_t timestamp;
            cef_geoposition_error_code_t error_code;
            cef_string_t error_message;
        } cef_geoposition_t; -}
#starttype cef_geoposition_t
#field latitude , CDouble
#field longitude , CDouble
#field altitude , CDouble
#field accuracy , CDouble
#field altitude_accuracy , CDouble
#field heading , CDouble
#field speed , CDouble
#field timestamp , <cef_time_t>
#field error_code , <cef_geoposition_error_code_t>
#field error_message , <cef_string_utf16_t>
#stoptype
