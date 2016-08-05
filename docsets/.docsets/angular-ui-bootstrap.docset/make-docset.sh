#!/bin/bash

set -o errexit
set -o nounset

docdir="$HOME/.docsets/angular-ui-bootstrap.docset/Contents/Resources/Documents"
mkdir -p "$docdir"
cd "$docdir"

cat <<EOF > ../../Info.plist
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleIdentifier</key>
    <string>nginx</string>
    <key>CFBundleName</key>
    <string>Nginx</string>
    <key>DocSetPlatformFamily</key>
    <string>nginx</string>
    <key>isDashDocset</key>
    <true/>
</dict>
</plist>
EOF

rm ../docSet.dsidx

sqlite3 ../docSet.dsidx "CREATE TABLE searchIndex(id INTEGER PRIMARY KEY, name TEXT, type TEXT, path TEXT);"
sqlite3 ../docSet.dsidx "CREATE UNIQUE INDEX anchor ON searchIndex (name, type, path);"

cat <<EOF | sqlite3 ../docSet.dsidx
INSERT OR IGNORE INTO searchIndex(name, type, path) VALUES
('Accordion', 'directive', 'https://angular-ui.github.io/bootstrap/index.html#accordion'),
('Alert', 'directive', 'https://angular-ui.github.io/bootstrap/index.html#alert'),
('Buttons', 'directive', 'https://angular-ui.github.io/bootstrap/index.html#buttons'),
('Carousel', 'directive', 'https://angular-ui.github.io/bootstrap/index.html#carousel'),
('Collapse', 'directive', 'https://angular-ui.github.io/bootstrap/index.html#collapse'),
('Dateparser', 'directive', 'https://angular-ui.github.io/bootstrap/index.html#dateparser'),
('Datepicker', 'directive', 'https://angular-ui.github.io/bootstrap/index.html#datepicker'),
('Datepicker Popup', 'directive', 'https://angular-ui.github.io/bootstrap/index.html#datepickerPopup'),
('Dropdown', 'directive', 'https://angular-ui.github.io/bootstrap/index.html#dropdown'),
('Modal', 'directive', 'https://angular-ui.github.io/bootstrap/index.html#modal'),
('Pager', 'directive', 'https://angular-ui.github.io/bootstrap/index.html#pager'),
('Pagination', 'directive', 'https://angular-ui.github.io/bootstrap/index.html#pagination'),
('Popover', 'directive', 'https://angular-ui.github.io/bootstrap/index.html#popover'),
('Position', 'directive', 'https://angular-ui.github.io/bootstrap/index.html#position'),
('Progressbar', 'directive', 'https://angular-ui.github.io/bootstrap/index.html#progressbar'),
('Rating', 'directive', 'https://angular-ui.github.io/bootstrap/index.html#rating'),
('Tabs', 'directive', 'https://angular-ui.github.io/bootstrap/index.html#tabs'),
('Timepicker', 'directive', 'https://angular-ui.github.io/bootstrap/index.html#timepicker'),
('Tooltip', 'directive', 'https://angular-ui.github.io/bootstrap/index.html#tooltip'),
('Typeahead', 'directive', 'https://angular-ui.github.io/bootstrap/index.html#typeahead');
EOF
