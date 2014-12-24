/// sessions
require("session.js");
session_auto_save_auto_load = true;

session_pref("signon.rememberSignons", true);
session_pref("signon.expireMasterPassword", false);
session_pref("signon.SignonFileName", "signons.txt");
Cc["@mozilla.org/login-manager;1"].getService(Ci.nsILoginManager); // init

/// variables
minibuffer_completion_rows = 20

/// modifiers
modifiers.M = new modifier(
  function (event) { return event.metaKey; },
  function (event) { event.metaKey = true; });

/// emacs as external editor
editor_shell_command = "emacsclient -c";

/// global settings/switches
mode_line_mode(false);

/// buffers ordered by recency
interactive("switch-to-recent-buffer",
    "Prompt for a buffer and switch to it, displaying the list in last-visited order.",
            function (I) {
        switch_to_buffer(
            I.window,
            (yield I.minibuffer.read_buffer(
                $prompt = "Switch to buffer:",
                $buffers = I.window.buffers.buffer_history,
                $default = (I.window.buffers.count > 1 ?
                            I.window.buffers.buffer_history[1] :
                            I.buffer))));
    });

define_key(default_global_keymap, "C-'", "switch-to-recent-buffer");
define_key(default_global_keymap, "M-k", "kill-current-buffer");

/// keys
define_key(content_buffer_normal_keymap, "v", "follow-new-buffer");
define_key(content_buffer_normal_keymap, "V", "follow-new-buffer-background");

define_key(content_buffer_normal_keymap, "o", "find-url-new-buffer");
define_key(content_buffer_normal_keymap, "O", "find-url");

define_key(content_buffer_normal_keymap, "j", "cmd_scrollLineDown");
define_key(content_buffer_normal_keymap, "k", "cmd_scrollLineUp");

define_key(content_buffer_normal_keymap, "J", "cmd_scrollPageDown");
define_key(content_buffer_normal_keymap, "K", "cmd_scrollPageUp");

undefine_key(content_buffer_normal_keymap, "g");
define_key(content_buffer_normal_keymap, "g g", "cmd_scrollTop");
define_key(content_buffer_normal_keymap, "G", "cmd_scrollBottom");

/// DOM/element selections
require("element.js");
define_browser_object_class("list",
                            "Browser object class for selecting a list node via hinting.",
                            xpath_browser_object_handler("//ul | //ol"),
                            $hint = "select (un)ordered list");

define_key(content_buffer_normal_keymap, "* l", "browser-object-list");

/// helpers
interactive("switch-to-other-buffer",
            "Switch to the previously open buffer",
            function (I) {
                var blist = I.window.buffers.buffer_history;
                if (blist.length > 1)
                  switch_to_buffer(I.window, blist[1]);
            });
define_key(default_global_keymap, "C-x C-a", "switch-to-other-buffer");


/// revive a killed buffer
var kill_buffer_original = kill_buffer_original || kill_buffer;

var killed_buffer_urls = [];
var killed_buffer_histories = [];

//  remember_killed_buffer
kill_buffer = function (buffer, force) {
    var hist = buffer.web_navigation.sessionHistory;

    if (buffer.display_uri_string && hist) {
        killed_buffer_histories.push(hist);
        killed_buffer_urls.push(buffer.display_uri_string);
    }

    kill_buffer_original(buffer,force);
};

interactive("revive-buffer",
    "Loads url from a previously killed buffer",
    function restore_killed_buffer (I) {
        if (killed_buffer_urls.length !== 0) {
            var url = yield I.minibuffer.read(
                $prompt = "Restore killed url:",
                $completer = all_word_completer($completions = killed_buffer_urls),
                $default_completion = killed_buffer_urls[killed_buffer_urls.length - 1],
                $auto_complete = "url",
                $auto_complete_initial = true,
                $auto_complete_delay = 0,
                $match_required);

            var window = I.window;
            var creator = buffer_creator(content_buffer);
            var idx = killed_buffer_urls.indexOf(url);

            // Create the buffer
            var buf = creator(window, null);

            // Recover the history
            buf.web_navigation.sessionHistory = killed_buffer_histories[idx];

            // This line may seem redundant, but it's necessary.
            var original_index = buf.web_navigation.sessionHistory.index;
            buf.web_navigation.gotoIndex(original_index);

            // Focus the new tab
            window.buffers.current = buf;

            // Remove revived from cemitery
            killed_buffer_urls.splice(idx, 1);
            killed_buffer_histories.splice(idx, 1);
        } else {
            I.window.minibuffer.message("No killed buffer urls");
        }
    });
define_key(default_global_keymap, "C-T", "revive-buffer");

/// remember download file path
{
   let _save_path = get_home_directory();

   function update_save_path (info) {
       _save_path = info.target_file.parent.path;
   }

   add_hook("download_added_hook", update_save_path);

   suggest_save_path_from_file_name = function (filename, buffer) {
       let file = make_file(_save_path);
       file.append(filename);
       return file.path;
   }
}

/// webjumps
define_opensearch_webjump("w", "wikipedia.xml");
define_opensearch_webjump("wg", "wikipedia-de.xml");
define_opensearch_webjump("wi", "wikipedia-it.xml");
define_opensearch_webjump("wcs", "wikipedia-cs.xml");
define_opensearch_webjump("wl", "wikipedia-la.xml");
define_opensearch_webjump("wr", "wikipedia-ru.xml");
define_opensearch_webjump("wd", "wiktionary.xml");
define_opensearch_webjump("y", "youtube.xml");

define_webjump("imdb", "http://www.imdb.com/find?q=%s");
define_webjump("r", "http://reddit.com/r/%s");
define_webjump("wa", "http://www.wolframalpha.com/input/?i=%s");
define_webjump("gi", "http://www.google.com/images?q=%s");
define_webjump("wf", "http://fallout.wikia.com/wiki/index.php?search=%s&fulltext=0");
define_webjump("e", "http://www.etymonline.com/index.php?allowed_in_frame=0&search=%s&searchmode=none");
define_webjump("tpb", "http://thepiratebay.se/s/?q=%s&page=0&orderby=99");
define_webjump("juls", "http://slovnik.juls.savba.sk/?w=%s&s=exact&c=ia8c&d=kssj4&d=psp&ie=utf-8&oe=utf-8");
define_webjump("l", "http://linguax.com/lexica/old.php?searchedLG=%s");
define_webjump("ud", "http://www.urbandictionary.com/define.php?term=%s");
define_webjump("lide", "https://is.muni.cz/auth/lide/?searchid=%s&Hledat=Hledat");
define_webjump("df", "http://df.magmawiki.com/index.php?search=%s&go=Go");
