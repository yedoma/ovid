# Installing and upgrading ovid

## The download for your computer

Each release has one source archive and one runtime per computer. Download both, then unpack them as below.

| Computer | Download | Unpack |
|---|---|---|
| Mac with Apple silicon (M1 and later) | `ovid-apps-<version>.tar.gz` and `ovid-runtime-mac-arm64.tar.gz` | Unpack the apps, then the runtime into the apps folder and rename that folder `runtime` |
| Mac with an Intel processor | `ovid-apps-<version>.tar.gz` and `ovid-runtime-mac-x86_64.tar.gz` | As above |
| Windows 10 or 11 (64-bit) | `ovid-apps-<version>.tar.gz` and `ovid-runtime-win-x64.zip` | As above (Windows 11 opens .tar.gz; on Windows 10 use 7-Zip) |

With `runtime` beside `Open ovid.command` (Mac) or `Open ovid.bat` (Windows), nothing else needs installing: R is not needed.
On a Mac, the first time, right-click `Open ovid.command` and choose Open. Not sure which Mac you have? Apple menu > About This Mac:
"Chip: Apple M…" is Apple silicon, "Processor: Intel…" is Intel.

## A first look with the sample data

Each app has a small sample in its folder, `<APP>/tests/golden/raw` (for example `CS/tests/golden/raw`). Open the app, choose
that folder as the raw data folder and a new empty folder as the project, and run. The sample is only read, never changed.
When that works, choose your own archive.

## Upgrading

1. Keep the old apps folder until the new one has run your projects once.
2. Before upgrading, let any run finish. If the app says a run did not finish, open the project once in the version that
   started it so it can put the output back.
3. Open each project in the new version and run it. The first run redoes cached steps (the versions changed), so it takes
   longer than an incremental run.

Projects made by v1.0.0 open and run in this version, and a project run by this version still opens and runs in v1.0.0
(checked with each app's sample, 2026-11-03). The safe way back to an older version: let any run finish (no
`.ovid_run`, `.output_before_run` or `.output_failed` in the project folder), then open the project in the old version.
Do not open in an older version a project whose run is unfinished: the older version does not read the newer run journal.

## When a run will not start because of the output size

Before a run changes output/, ovid keeps a copy of the last finished output, so a stopped or failed run can be undone.

- On a Mac's own disk (APFS) the copy is a clone: instant, taking no space until files change. No limit applies.
- On other drives (external drives formatted exFAT or FAT32, network drives, Windows) the copy is a full copy. ovid stops
  before a run when the output is over 2 GB on such a drive, or when the drive has less free space than twice the output
  plus 500 MB. The message says which one applies.

What to do: move the project folder to the Mac's own disk; or make the output smaller (fewer outputs, or split the project
by years); or, for the free-space message, free space on that drive. The raw data folder can stay where it is.

## Sending a troubleshooting zip

The lifebuoy in the top bar saves a zip to send with a line on what went wrong. It holds, each listed in its README.txt:

- `versions.txt`: app, core, R, operating system and package versions, and the project folder;
- `logs/`: the newest 10 run logs and the app's own error log;
- `audit/`: problems, the run manifest and history, reconciliation and the last set-up check; `setup_check_now.csv`;
- `config/`: the project's set-up tables (station names, sites, serials, coordinates and notes are in these), with columns
  that look like passwords or tokens left out; `config_changes.csv`: when each was saved.

It holds no raw data and no output data files; your home folder's name is replaced by `~`, but other folder names and the
contents of the set-up tables are as they are. Files over 5 MB are listed, not included. Open the zip and look before you
send it.
