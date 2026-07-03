/*** Required for userChrome.css / userContent.css ***/
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);

/*** Dark mode — respect system preference (set by gsettings color-scheme) ***/
user_pref("layout.css.prefers-color-scheme.content-override", 2);
user_pref("browser.theme.dark-toolbar-theme", true);
user_pref("browser.theme.content-theme", 0);

/*** Optional: reduce annoyances (keep minimal) ***/
user_pref("browser.tabs.warnOnClose", false);
user_pref("browser.tabs.warnOnCloseOtherTabs", false);
