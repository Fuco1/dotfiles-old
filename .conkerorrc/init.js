modifiers.M = new modifier(
  function (event) { return event.metaKey; },
  function (event) { event.metaKey = true; });

require("element.js");
define_browser_object_class("list",
                            "Browser object class for selecting a list node via hinting.",
                            xpath_browser_object_handler("//ul | //ol"),
                            $hint = "select (un)ordered list");

define_key(content_buffer_normal_keymap, "* l", "browser-object-list");
