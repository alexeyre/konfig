c.fonts.default_family = ["FiraCode NF", "FiraCode Nerd Font", "FiraCode Nerd Font Mono", "monospace"]
c.tabs.position = "left"
c.colors.tabs.bar.bg = "#1b1d1e"
c.colors.statusbar.normal.bg = "#1b1d1e"
c.colors.completion.category.bg = "#404040"
c.colors.statusbar.command.bg = "#1b1d1e"
c.colors.statusbar.normal.fg = "#fc20bb"
c.colors.statusbar.url.success.https.fg = "#fc20bb"
c.colors.completion.even.bg = "#1b1d1e"
c.colors.completion.odd.bg = "#1b1d1e"
c.colors.completion.fg = "#dddddd"
c.tabs.show = "multiple"
c.tabs.last_close = "default-page"

#c.content.user_stylesheets = "notion.so.css"

c.qt.args = ["force-webrtc-ip-handling-policy=disable_non_proxied_udp"]
c.content.webrtc_ip_handling_policy = "disable-non-proxied-udp"

config.bind(",m", "hint links spawn /usr/local/bin/iina '{hint-url}'")
config.bind(";m", "hint --rapid links spawn /usr/local/bin/iina '{hint-url}'")
config.bind("zm", "spawn /usr/local/bin/iina '{url}'")

c.downloads.location.directory = "~/downloads"


c.auto_save.session = True

c.content.host_blocking.lists.append(
    "https://block.energized.pro/unified/formats/hosts"
)
c.content.host_blocking.whitelist.append("*.4chan.org")
c.content.host_blocking.whitelist.append("*.4cdn.org")

c.input.insert_mode.leave_on_load = False

c.hints.chars = "aoeuidhtns"
config.bind("a", "hint all normal")
config.bind("A", "hint all tab-fg")
config.bind("E", "hint all tab-bg")

config.bind("<z><l>", "spawn --userscript qute-pass --mode gopass")
config.bind("<z><u><l>", "spawn --userscript qute-pass --mode gopass --username-only")
config.bind("<z><p><l>", "spawn --userscript qute-pass --mode gopass --password-only")
config.bind("<z><o><l>", "spawn --userscript qute-pass --mode gopass --otp-only")

c.url.searchengines = {
    "DEFAULT": "https://google.co.uk/search?query={}",
    "lk": "https://duckduckgo.com/?kl=uk-en&q=%5C{}",
    "d": "https://duckduckgo.com/?q={}",
    "yt": "https://youtube.com/results?search_query={}",
    "scholar": "https://scholar.google.com/scholar?q={}",
    "wiki": "https://en.wikipedia.org/w/index.php?search={}",
    "amaz": "https://amazon.co.uk/s?k={}",
    "dict": "https://en.wiktionary.org/w/index.php?search={}",
    "gitlab": "https://gitlab.com/search?search={}",
    "gitlabcc": "https://gitlab.uk.cambridgeconsultants.com/search?search={}",
    "ccwiki": "https://wiki.uk.cambridgeconsultants.com/index.php?search={}",
    "github": "https://github.com/search?q={}",
    "cmc": "https://coinmarketcap.com/currencies/{}",
    "sec": "https://www.sec.gov/cgi-bin/browse-edgar?company=&match=&CIK={}&filenum=&State=&Country=&SIC=&owner=exclude&Find=Find+Companies&action=getcompany",
}

config.bind("xt", "config-cycle tabs.show never multiple")
config.bind("xn", "config-cycle statusbar.hide false true")

c.fonts.statusbar = "14pt default_family"
c.fonts.completion.category = "bold 14pt default_family"
c.fonts.completion.entry = "14pt default_family"
c.fonts.downloads = "14pt default_family"
