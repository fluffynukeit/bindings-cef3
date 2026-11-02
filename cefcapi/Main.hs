-- This program uses the bindings-cef3 package to create a browser window
-- and load a URL.  It has been modeled after the C CEF API program cefcapi
-- by CzarekTomczak at https://github.com/CzarekTomczak/cefcapi as of
-- commit ba390dd03cb59e965960f2b62a68d4938ffda3ec.  Analogous variables
-- and function have been given similar or identical names.
--
-- In effort to keep the parallel to the model C program close, not much
-- care is paid to freeing allocated resources.  Proper implementation
-- would look after things like that.
--
-- Note that to run the program, you will need a locale .pak file located
-- in the locales directory.  Pak files are available in the CEF
-- distribution.  If you don't have the right file, you'll see an error
-- similar to
-- [1106/071615:FATAL:main_delegate.cc(499)] Check failed:
-- !loaded_locale.empty(). Locale could not be found for en-US

module Main where

import           Bindings.CEF3
import           Control.Applicative
import           Control.Monad
import           Foreign.C
import           Foreign.Marshal      hiding (void)
import           Foreign.Ptr
import           Foreign.Storable
import           Gtk
import           System.Environment
import           System.Posix.Process

main :: IO ()
main = do
  printPID "Starting main"

  -- Args passed to main_args MUST include the program name because CEF
  -- is configured by default to start a new instance of the running
  -- executable.  Different instances handle different tasks.
  haskArgs <- getArgs
  argv <- getProgName >>= return . (:haskArgs) >>= mapM newCString
  let argc = fromIntegral . length $ argv
  pargv <- newArray argv
  mainArgs <- new $ C'cef_main_args_t argc pargv

  -- Build a complete (but useless) cef_app_t.  The app type configures
  -- "global" behaviors like resource loading.
  app <- initialize_app_handler

  -- cef_execute_process examines the command line arguments to determine
  -- its behavior.  If there is no "type" argument, as is the case with the
  -- "browser" process, it returns immediately with -1. Otherwise, its
  -- behavior is determined by the command line flags.  By default, CEF
  -- will execute additional instances of the executable and set command
  -- line flags automatically for required subprocesses.
  printPID $ "cef_execute_process, argc="++ show argc ++ ", argv="++ show haskArgs
  exCode <- c'cef_execute_process mainArgs app nullPtr
  unless (exCode >= 0) $ do

    -- At this point, we know we are in the browser process.  Continue
    -- standard initialization.

    -- cef_settings_t configures how CEF processes are run, where resources
    -- are loaded from, etc.
    settings <- mkCefSettings
    printPID "cef_initialize"
    void $ c'cef_initialize mainArgs settings app nullPtr

    -- Build the GTK window that will host the CEF browser widget, which
    -- must have a GTKBox or subclass as its parent.  A small set of GTK
    -- bindings are included in the GTK module.
    initialize_gtk
    winTitle <- newCString "cefcapi example"
    hwnd <- create_gtk_window winTitle 1024 768
    windowInfo <- new $ C'cef_window_info_t hwnd 0 0 nullPtr

    -- Our target URL.  file:/// and other supported browser types work
    -- here (I think).  If the target URL is empty (i.e. ""), the browser
    -- will NOT render anything and instead you'll see an empty GTK window.
    cefUrl <-  mkCefStringPtr "http://www.google.com"

    -- Browser settings configure fonts, javascript, plugins, cache, webgl,
    -- etc.
    browserSettings <- mkBrowserSettings

    -- The client acts as an interface between the host application and the
    -- browser.  The client contains callbacks for downloads, context menu,
    -- drag, etc.
    client <- initialize_client_handler

    -- Create a browser in the window described by windowInfo, with
    -- application callbacks defined in the client, load a given URL, use
    -- the provided browser settings, and null "request context."
    printPID "cef_browser_host_create_browser"
    void $ c'cef_browser_host_create_browser
      windowInfo
      client
      cefUrl
      browserSettings
      nullPtr

    -- Now communicate  messages between processes until shutdown.
    printPID "cef_run_message_loop"
    c'cef_run_message_loop
    printPID "cef_shutdown"
    c'cef_shutdown


-- This function is useful for printing a status message in addition to the
-- PID of the process.  Since CEF launches additional processes
-- automatically, having the PID helps when figuring out which is doing
-- what.
printPID :: String -> IO ()
printPID str = do
  pid <- getProcessID
  print $ show pid ++ " " ++  str


initialize_app_handler :: IO (Ptr C'cef_app_t)
initialize_app_handler = do
  printPID "initialize_app_handler"
  newWithSize (C'cef_app_t
    <$> initialize_cef_base
    <*> mk'cb_cef_app_on_before_command_line_processing (rtVoid3 "on_before_command_line_processing")
    <*> mk'cb_cef_app_on_register_custom_schemes        (rtVoid2 "on_register_custom_schemes")
    <*> mk'cb_cef_app_get_resource_bundle_handler       (rtNull1 "get_resource_bundle_handler")
    <*> mk'cb_cef_app_get_browser_process_handler       (rtNull1 "get_browser_process_handler")
    <*> mk'cb_cef_app_get_render_process_handler        (rtNull1 "get_render_process_handler")
    )

-- Allocate space for a new object, build the object, copy the object to
-- the space, then store the size of the object.  In CEF, size is always at
-- byte offset 0.
newWithSize :: Storable a => IO a -> IO (Ptr a)
newWithSize mkObj = do
  obj <- mkObj
  let sz = sizeOf obj
  printPID $ "Size: "++ show sz
  ptr <- new obj
  pokeByteOff ptr 0 sz
  return ptr

initialize_cef_base :: IO C'cef_base_t
initialize_cef_base = do
  printPID "initialize_cef_base"
  let sz = 0
  C'cef_base_t
    <$> return sz
    <*> mk'cb_cef_base_add_ref    (rtInt1 "cef_base_t.add_ref")
    <*> mk'cb_cef_base_release    (rtInt1 "cef_base_t.release")
    <*> mk'cb_cef_base_get_refct  (rtInt1 "cef_base_t.get_refct")

-- Helpers for building dummy callbacks (return void, return NULL, etc)
-- These dummy callbacks do nothing put print their PID and strings to
-- screen.  In practice, instead of these printing callbacks, undesired
-- callbacks can be given nullFunPtr instead of invoking a mk'blah function
-- to build the callback.
rtNull1 :: String -> a -> IO (Ptr b)
rtNull1 s _ = printPID s >> return nullPtr

rtVoid2 :: String -> a -> b ->  IO ()
rtVoid2 s _ _ = printPID s >> return ()

rtVoid3 :: String -> a -> b -> c -> IO ()
rtVoid3 s _ _ _ = printPID s >> return ()

rtInt1 :: String -> a -> IO CInt
rtInt1 s _ = printPID s >> return 1


-- Settings for configuring CEF process behaviors
mkCefSettings :: IO (Ptr C'cef_settings_t)
mkCefSettings = newWithSize
  (C'cef_settings_t
  <$> return 0 -- dummy size, newWithSize will populate
  <*> return 0 -- single process
  <*> return 1 -- no_sandbox
  <*> mkCefString ""
  <*> return 0 -- multithreaded message loop
  <*> return 0 -- command line args disabled
  <*> mkCefString ""
  <*> return 0 -- persist session cookies
  <*> mkCefString ""
  <*> mkCefString ""
  <*> mkCefString ""-- locale
  <*> mkCefString ""--log file
  <*> return c'LOGSEVERITY_DEFAULT
  <*> return 0 -- release dcheck enabled
  <*> mkCefString ""
  <*> mkCefString ""
  <*> mkCefString ""-- locales dir path
  <*> return 0 -- pack loading disabled
  <*> return 0 -- remote debugging port
  <*> return 5 -- uncaught exception stack size
  <*> return 0
  <*> return 0 -- ignore certificate errors
  <*> return 0 -- background color
  )

-- Build a CEF utf16 string in memory from a Haskell string.
mkCefStringPtr :: String -> IO (Ptr C'cef_string_utf16_t)
mkCefStringPtr str = do
  let sz = fromIntegral . length $ str
  strC <- newCString str

  -- We need to allocate and *initialize* a space for the utf16 string.
  -- CEF will try to call its destructor if it has one, so failure to
  -- initialize results in segfault.
  pUtf16 <- new $ C'cef_string_utf16_t nullPtr 0 nullFunPtr
  void $ c'cef_string_utf8_to_utf16 strC sz pUtf16
  return pUtf16

mkCefString :: String -> IO C'cef_string_utf16_t
mkCefString str = mkCefStringPtr str >>= peek


-- GTK helper functions.  These mostly match cefcapi.
initialize_gtk :: IO ()
initialize_gtk = do
  printPID "initialize_gtk"
  c'gtk_init nullPtr nullPtr

create_gtk_window :: CString -> CInt -> CInt -> IO (Ptr C'GtkWidget)
create_gtk_window title w h = do
  printPID "create_gtk_window"
  window <- c'gtk_window_new c'GTK_WINDOW_TOPLEVEL
  c'gtk_window_set_default_size window w h
  c'gtk_window_set_title window title
  vbox <- c'gtk_vbox_new 0 0
  c'gtk_container_add window vbox
  c'gtk_widget_show_all window
  return vbox

-- Settings for configuring the browser behavior
mkBrowserSettings :: IO (Ptr C'cef_browser_settings_t)
mkBrowserSettings = newWithSize
  (C'cef_browser_settings_t
  <$> return 0  -- dummy size, newWithSize will populate
  <*> mkCefString ""
  <*> mkCefString ""
  <*> mkCefString ""
  <*> mkCefString ""
  <*> mkCefString ""
  <*> mkCefString ""
  <*> return 14 -- default font size
  <*> return 10
  <*> return 10
  <*> return 10
  <*> mkCefString ""-- default encoding
  <*> return c'STATE_DEFAULT -- remove_fonts
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return c'STATE_DEFAULT
  <*> return 0 -- background color
  )

-- Callbacks for events within the browser.
initialize_client_handler :: IO (Ptr C'cef_client_t)
initialize_client_handler = do
  printPID "initialize_client_handler"
  newWithSize
    (C'cef_client_t
    <$> initialize_cef_base
    <*> mk'cb_cef_client_get_context_menu_handler    (rtNull1 "get_context_menu_handler")
    <*> mk'cb_cef_client_get_dialog_handler          (rtNull1 "get_dialog_handler")
    <*> mk'cb_cef_client_get_display_handler         (rtNull1 "get_display_handler")
    <*> mk'cb_cef_client_get_download_handler        (rtNull1 "get_download_handler")
    <*> mk'cb_cef_client_get_drag_handler            (rtNull1 "get_drag_handler")
    <*> mk'cb_cef_client_get_focus_handler           (rtNull1 "get_focus_handler")
    <*> mk'cb_cef_client_get_geolocation_handler     (rtNull1 "get_geolocation_handler")
    <*> mk'cb_cef_client_get_jsdialog_handler        (rtNull1 "get_jsdialog_handler")
    <*> mk'cb_cef_client_get_keyboard_handler        (rtNull1 "get_keyboard_handler")
    <*> mk'cb_cef_client_get_life_span_handler       (rtNull1 "get_life_span_handler")
    <*> mk'cb_cef_client_get_load_handler            (rtNull1 "get_load_handler")
    <*> mk'cb_cef_client_get_render_handler          (rtNull1 "get_render_handler")
    <*> mk'cb_cef_client_get_request_handler         (rtNull1 "get_request_handler")
    <*> mk'cb_cef_client_on_process_message_received
          (\_ _ _ _ -> printPID "on_message_process_received" >> return 0)
    )
