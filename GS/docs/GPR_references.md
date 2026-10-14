# GPR viewer: references and what each one set

The GPR page (`R/gpr.R`) shows MALA lines with processing and velocities taken from these sources. The same list is on the page
(GPR > References). Raw files are never changed; every step runs on a copy for display, and the page prints the steps it applied.

## References
1. Gusmeroli, A., Liu, L., Schaefer, K. M., Zhang, T., Schaefer, T. and Grosse, G. (2015). Active layer stratigraphy and organic layer thickness at a thermokarst site in Arctic Alaska identified using ground penetrating radar. Arctic, Antarctic, and Alpine Research 47(2). doi:10.1657/AAAR00C-13-301. Peat 0.039, silt 0.054 and whole active layer 0.045 m/ns (CMP); 500 MHz; dewow, background removal, Ormsby band-pass, time-scaled gain; thaw depth within 5 cm of probing.
2. Wollschlager, U., Gerhards, H., Yu, Q. and Roth, K. (2010). Multi-channel ground-penetrating radar to explore spatial variations in thaw depth and moisture content in the active layer of a permafrost site. The Cryosphere 4, 269-283. doi:10.5194/tc-4-269-2010. Permittivity of permafrost 4-5 and of wet sand / gravel 10-20 (after Sharma 1997); 0.08 m/ns for permittivity 15; 200 MHz; dewow only; a constant velocity moved the frost table 0.5 m under a vegetated patch.
3. Moorman, B. J., Robinson, S. D. and Burgess, M. M. (2003). Imaging periglacial conditions with ground-penetrating radar. Permafrost and Periglacial Processes 14(4), 319-329. doi:10.1002/ppp.463. Review of over 100 permafrost surveys: the frozen / unfrozen interface gives a strong, laterally coherent reflection.
4. Sandmeier, K. J. REFLEXW manual, version 10.3 (sandmeier-geo.de/Download/reflexw_manual.pdf), and 'Introduction to the processing of GPR-data within REFLEXW' (sandmeier-geo.de/Download/gpr_2d_import_processing.pdf): order of steps, dewow window, time zero, energy decay, gain function, AGC, background removal, band-pass, time-depth conversion, MALA RD3 / RD7 and .cor / .mrk import.
5. MALA GeoScience. GroundVision Software Operating Manual v. 1.4 (2009): file types (.rd3 16-bit, .rad, .cor, .mrk), the .cor layout, DC filter, time-gain filter, subtract mean trace (removes horizontal and nearly horizontal features), sampling frequency about 10 x antenna frequency, about 500 samples.
6. Davis, J. L. and Annan, A. P. (1989). Ground-penetrating radar for high-resolution mapping of soil and rock stratigraphy. Geophysical Prospecting 37(5), 531-551. Velocity depends on the permittivity, which depends on water and ice content.

## What came from where
| Setting | Value | Source |
| --- | --- | --- |
| Order of steps | DC / dewow, time zero, band-pass, gain, background removal | REFLEXW manual 0.15.6.4 and guide II.1-II.4; GroundVision manual 3.3.2 |
| Dewow window | 1 period of the antenna frequency (0 to 5 selectable) | REFLEXW manual 1.11.1.8 (about one period); guide II.1 (one or two) |
| Time zero | first zero crossing before the first strong extremum of the mean trace | REFLEXW guide II.2 (first zero crossing, then move start time) |
| Band-pass | Ormsby, corners 0.25 / 1 / 2 / 4 x antenna frequency | Gusmeroli et al. 2015 (125-500 / 1000-2000 MHz on 500 MHz) |
| Gain | energy decay (no parameters); gain function (1 + a t) exp(b t) | REFLEXW manual 1.11.2.2, 1.11.2.3; GroundVision time-gain filter |
| Background removal | mean trace or moving mean of N traces; off by default | GroundVision 1.5 (removes horizontal and nearly horizontal features); REFLEXW guide II.4 |
| Velocity, frozen ground | 0.141 m/ns (permittivity 4-5, middle 4.5) | Sharma 1997, as cited by Wollschlager et al. 2010 |
| Velocity, thawed wet sand / gravel | 0.0775 m/ns (permittivity 10-20, middle 15; 0.08 in the paper) | Wollschlager et al. 2010 |
| Velocity, peat / silt / whole active layer | 0.039 / 0.054 / 0.045 m/ns | Gusmeroli et al. 2015 (CMP) |
| Velocity from permittivity | v = 0.3 / sqrt(permittivity) | REFLEXW manual; Wollschlager et al. 2010 |
| Sampling frequency check | shown against the antenna; MALA advises about 10 x | GroundVision manual, measurement settings |
| File types | .rd3 16-bit (.rd7 32-bit), .rad, .cor, .mrk | GroundVision manual 1.3; REFLEXW manual import |

## Topography, markers and .partial copies
- Topography uses the height in the `.cor` file (GroundVision manual: "height above mean sea level"); REFLEXW's equivalent is "correct header elevations" (guide II.6).
- Markers: REFLEXW manual, import menu: a `.mrk` file has one header line and then the trace number of each marker (first column), written as distance markers; marker spacing is entered by the user ("marker inc."), guide I.2.2. The second and third numbers in the files here (30, 1) are not described in the manuals read.
- `.partial`: see `R/gpr.R` (comment above `gpr_scan`) and `Logger_tools/recover_partials.py`.

## Choices that are ours, not from a source
- DC removal uses the last quarter of each trace (GroundVision lets the user pick the interval; no default is given).
- The AGC window default (25 ns) and the moving background window (50 traces) are starting values.
- The Gusmeroli preset uses one period for the dewow and energy decay for the gain, because the paper names a dewow and a time-scaled gain
  without their parameters. Their order (dewow, background removal, band-pass, gain) differs from the order used here.
- Ice (about 0.168 m/ns) is not in the velocity list: no source for it was checked. Frozen-ground and active-layer velocities are values
  measured or assumed at specific sites; measure one on site (hyperbola fit, CMP or a probe depth to the frost table) and replace them.
- The thaw-depth line is typed in by hand; picking the frost table from the radargram is not done.

## Not yet read
The MALA RAMAC X3M hardware manual (manualslib.com / fcc.report copies) was found but not read for this version: it covers the
instrument, not the file format, which is taken from the GroundVision manual and the REFLEXW import notes. MALA Vision Desktop
(April 2026) was read for its analysis tools (hyperbola fitting, time zero, DC offset) only.
