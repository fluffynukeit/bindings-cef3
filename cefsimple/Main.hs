module Main where

import           Bindings.CEF3
import           Control.Applicative
import           Control.Monad
import           Foreign.C
import           Foreign.Marshal
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.IO.Encoding
import           Gtk
import           System.Environment
import           System.Posix.Process

main :: IO ()
main = do
  pid <- getProcessID >>= return . show
  getLocaleEncoding >>= print
  print $ pid ++ " Starting main"
  haskArgs <- getArgs
  argv <- getProgName >>= return . (:haskArgs) >>= mapM newCString
  let argc = fromIntegral . length $ argv
  pargv <- newArray argv
  mainArgs <- new $ C'cef_main_args_t argc pargv
  app <- initialize_app_handler
  print $ pid ++ " cef_execute_process, argc=" ++ show argc ++ ", argv=" ++ show haskArgs
  exCode <- c'cef_execute_process mainArgs app nullPtr
  print $ pid ++ " exCode: " ++ show exCode
  unless (exCode >= 0) $ do
    settings <- mkCefSettings
    print $ pid ++ " cef_initialize"
    c'cef_initialize mainArgs settings app nullPtr
    initialize_gtk
    winTitle <- newCString "cefcapi example"
    hwnd <- create_gtk_window winTitle 1024 768
    windowInfo <- new $ C'cef_window_info_t hwnd 0 0 nullPtr
    cefUrl <-  mkCefStringPtr "http://www.google.com"
    browserSettings <- mkBrowserSettings
    client <- initialize_client_handler
    print $ pid ++ " cef_browser_host_create_browser"
    browserCode <- c'cef_browser_host_create_browser
      windowInfo
      client
      cefUrl
      browserSettings
      nullPtr
    print browserCode
    print $ pid ++ " cef_run_message_loop"
    c'cef_run_message_loop
    print $ pid ++ " cef_shutdown"
    c'cef_shutdown


initialize_app_handler :: IO (Ptr C'cef_app_t)
initialize_app_handler = do
  pid <- getProcessID >>= return . show
  print $ pid ++ " initialize_app_handler"
  app <- newWithSize (C'cef_app_t
          <$> initialize_cef_base
          <*> mk'cb_cef_app_on_before_command_line_processing (rtVoid3 $ pid ++ " on_before_command_line_processing")
          <*> mk'cb_cef_app_on_register_custom_schemes        (rtVoid2 $ pid ++ " on_register_custom_schemes")
          <*> mk'cb_cef_app_get_resource_bundle_handler       (rtNull1 $ pid ++ " get_resource_bundle_handler")
          <*> mk'cb_cef_app_get_browser_process_handler       (rtNull1 $ pid ++ " get_browser_process_handler")
          <*> mk'cb_cef_app_get_render_process_handler        (rtNull1 $ pid ++ " get_render_process_handler")
          )
  return app

-- Allocate space for a new object, build the object, copy the object to
-- the space, then store the size of the object.
newWithSize :: Storable a => IO a -> IO (Ptr a)
newWithSize mkObj = do
  obj <- mkObj
  let sz = sizeOf obj
  print $ "Size: " ++ show sz
  ptr <- new obj
  pokeByteOff ptr 0 sz
  actSize <- peekByteOff ptr 0 :: IO CSize
  print $ "A Size: " ++ show actSize
  return ptr

initialize_cef_base :: IO C'cef_base_t
initialize_cef_base = do
  pid <- getProcessID >>= return . show
  print $ pid ++ " initialize_cef_base"
  let sz = 0
  C'cef_base_t
    <$> return sz
    <*> mk'cb_cef_base_add_ref    (rtInt1 $ pid ++ " cef_base_t.add_ref")
    <*> mk'cb_cef_base_release    (rtInt1 $ pid ++ " cef_base_t.release")
    <*> mk'cb_cef_base_get_refct  (rtInt1 $ pid ++ " cef_base_t.get_refct")

-- Helpers for building dummy callbacks (return void, return NULL, etc)
rtNull1 :: String -> a -> IO (Ptr b)
rtNull1 s a = print s >> return nullPtr
rtVoid2 :: String -> a -> b ->  IO ()
rtVoid2 s a b = print s >> return ()
rtVoid3 :: String -> a -> b -> c -> IO ()
rtVoid3 s a b c = print s >> return ()
rtInt1 :: String -> a -> IO CInt
rtInt1 s a = print s >> return 1

mkCefSettings :: IO (Ptr C'cef_settings_t)
mkCefSettings = newWithSize
  (C'cef_settings_t
  <$> return 0 -- dummy size, newWithSize will populate
  <*> return 1 -- single process
  <*> return 1 -- no_sandbox
  <*> mkCefString "thisisatest"
  <*> return 0 -- multithreaded message loop
  <*> return 0 -- command line args disabled
  <*> mkCefString ""
  <*> return 0 -- persist session cookies
  <*> mkCefString ""
  <*> mkCefString ""
  <*> mkCefString "en-US" -- locale
  <*> mkCefString "./ceflog.txt" --log file
  <*> return c'LOGSEVERITY_DEFAULT
  <*> return 0 -- release dcheck enabled
  <*> mkCefString ""
  <*> mkCefString ""
  <*> mkCefString "dist/build/cefsimple/locales" -- locales dir path
  <*> return 0 -- pack loading disabled
  <*> return 0 -- remote debugging port
  <*> return 5 -- uncaught exception stack size
  <*> return 0
  <*> return 0 -- ignore certificate errors
  <*> return 0 -- background color
  )

mkCefStringPtr :: String -> IO (Ptr C'cef_string_utf16_t)
mkCefStringPtr str = do
  let sz = fromIntegral . length $ str
  print "Making cefString ptr"
  print "converting to c string"
  strC <- newCString str
  print "malloc"
  pUtf16 <- mallocBytes 10000 :: IO (Ptr C'cef_string_utf16_t)
  print "Converting"
  print sz
  return strC >>= peekCString >>= print
  c'cef_string_utf8_to_utf16 strC sz pUtf16
  print "returning"
  return pUtf16


mkCefString :: String -> IO C'cef_string_utf16_t
mkCefString str = mkCefStringPtr str >>= peek


initialize_gtk :: IO ()
initialize_gtk = do
  pid <- getProcessID >>= return . show
  print $ pid ++ " initialize_gtk"
  c'gtk_init nullPtr nullPtr

create_gtk_window :: CString -> CInt -> CInt -> IO (Ptr C'GtkWidget)
create_gtk_window title w h = do
  pid <- getProcessID >>= return . show
  print $ pid ++ " create_gtk_window"
  window <- c'gtk_window_new c'GTK_WINDOW_TOPLEVEL
  c'gtk_window_set_default_size window w h
  c'gtk_window_set_title window title
  labS <- newCString "test label"
  vbox <- c'gtk_vbox_new 0 0
  c'gtk_container_add window vbox
  c'gtk_widget_show_all window
  return vbox


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
  <*> mkCefString "" -- default encoding
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

initialize_client_handler :: IO (Ptr C'cef_client_t)
initialize_client_handler = do
  pid <- getProcessID >>= return . show
  print $ pid ++ "initialize_client_handler"
  newWithSize
    (C'cef_client_t
    <$> initialize_cef_base
    <*> mk'cb_cef_client_get_context_menu_handler    (rtNull1 $ pid ++ " get_context_menu_handler")
    <*> mk'cb_cef_client_get_dialog_handler          (rtNull1 $ pid ++ " get_dialog_handler")
    <*> mk'cb_cef_client_get_display_handler         (rtNull1 $ pid ++ " get_display_handler")
    <*> mk'cb_cef_client_get_download_handler        (rtNull1 $ pid ++ " get_download_handler")
    <*> mk'cb_cef_client_get_drag_handler            (rtNull1 $ pid ++ " get_drag_handler")
    <*> mk'cb_cef_client_get_focus_handler           (rtNull1 $ pid ++ " get_focus_handler")
    <*> mk'cb_cef_client_get_geolocation_handler     (rtNull1 $ pid ++ " get_geolocation_handler")
    <*> mk'cb_cef_client_get_jsdialog_handler        (rtNull1 $ pid ++ " get_jsdialog_handler")
    <*> mk'cb_cef_client_get_keyboard_handler        (rtNull1 $ pid ++ " get_keyboard_handler")
    <*> mk'cb_cef_client_get_life_span_handler       (rtNull1 $ pid ++ " get_life_span_handler")
    <*> mk'cb_cef_client_get_load_handler            (rtNull1 $ pid ++ " get_load_handler")
    <*> mk'cb_cef_client_get_render_handler          (rtNull1 $ pid ++ " get_render_handler")
    <*> mk'cb_cef_client_get_request_handler         (rtNull1 $ pid ++ " get_request_handler")
    <*> mk'cb_cef_client_on_process_message_received
          (\_ _ _ _ -> do print (pid ++ " on_message_process_received"); return 0)
    )
