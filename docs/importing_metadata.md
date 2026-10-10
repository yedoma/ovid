# Bringing in your own metadata

Metadata (which logger was where and when, site names, coordinates, depths, notes) comes into every Ovid app the same way: you bring a CSV, text or Excel file you already have, and say which column is which. Nothing in your file is changed.

Where: the **Import a spreadsheet** button on any set-up table (Loggers and metadata, Deployment sheet, Sensors and depths, Stations and others), or **Paste** to paste rows copied from a spreadsheet.

## What it can read
- **Files:** `.csv`, `.txt`, `.tsv`, `.xlsx`, `.xls`. A file is judged by what is inside it, not its name. Several sheets in one workbook: the one whose headings fit is read; choose another from the Sheet list.
- **Text:** UTF-8, UTF-16 and Windows-1252 (Excel on Windows); commas, semicolons, tabs or bars between columns; quoted cells with commas or line breaks in them.
- **Layout:** titles, notes and blank lines above the headings are skipped (if it picks the wrong row, open "The headings are not on the right row?" and give the row number); empty and repeated headings are made distinct; empty columns and rows are dropped; `N/A`, `-`, `null` and Excel errors (`#N/A`, `#REF!`) become blank.
- **Not read, with a plain message:** web pages saved as `.xls`, PDFs, pictures, `.ods`, damaged or password-protected workbooks, empty files, tables of more than 200,000 rows or 500 columns (the wrong file).

## Matching columns
Each heading is matched to a field by its name, its label or a known alternative (`SN`, `Serial no`, `Location name`, `Start date`, `Date installed` ...), ignoring case, spacing and punctuation. A unit or note in brackets is left out (`Latitude (dd)`), a longer heading that contains the words is found (`Logger serial number (HOBO)`), and a spelling slip is caught (`Serail`). Matches that are close but not exact are marked **check**. What you matched last time for the same headings is offered first ("as last time"). Change any match, or choose **Leave out**. One field takes one column only.

## Dates and numbers
- Dates are written into one form (`2024-08-31`, or `2024-08-31 14:30`) before they go in: day first or month first, with `/`, `.` or `-`, two-digit years, months in words (`31 Aug 2024`, `August 31st, 2024`), 12 and 24 hour times, and Excel's serial numbers (`45531`). A year alone (`2019`), a year and month (`2019-06`) and `Sept 2024` are kept as they are: the apps read them.
- If a date such as `03/04/2024` could be either order and nothing else in the column settles it, you are asked: **Dates written like 03/04/2024 are** day / month / year or month / day / year. A column with `31/08/2024` in it settles itself.
- Numbers: decimal commas (`61,30`), thousands separators (`1,234.5`, `1 234,5`, `1.234,5`), the typographic minus, and the field's own unit after the number (`45 cm`).
- Serial numbers that Excel has turned into `2.01E+07` cannot be repaired: it is an error, with how to fix it (format the column as Text and type them again). `123.0` becomes `123`.

## Before anything is written
The screen says what will happen, in three kinds of message:
- **Fix before importing** (red; the button is off): two columns on one field; dates whose order must be chosen; a serial in scientific notation; nothing matched; updating without the key column.
- **Check before importing** (amber; you can go ahead): rows with no key (they are added but left aside by the app), repeated keys, keys already in the table, values that are not dates, numbers or one of the choices (kept as typed; the app reads them as blank), and that **Use them instead of** replaces the current rows (Undo brings them back).
- **Notes** (grey): what was repaired or converted.
Below that is a preview of the first rows as they will be written.

## How the rows go in
**Add the rows**, **Update rows with the same key, add the rest** (a blank cell in your file never erases a filled one), or **Use them instead of the table**. Every change is kept in the table's history and can be undone.
