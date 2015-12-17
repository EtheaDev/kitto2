Ext JS wrapper for Object Pascal

0.9.8 Ext JS 3.2.1 support, 03-may-2010

ExtPascal is an Ext JS wrapper. 
ExtPascal lets you use the Ext JS from Object Pascal commands issued by the server using FastCGI or FastCGI thru CGI gateway. 
That brings the structure and strict syntax of the Object Pascal for programming the web browser.

Author: Wanderlan Santos dos Anjos. wanderlan.anjos@gmail.com

Home: http://extpascal.googlecode.com
Getting Started: http://code.google.com/p/extpascal/wiki/GettingStarted
Live demos: http://extpascal.call.inf.br/cgi-bin/extpascalsamples.cgi
Forum: http://groups.google.com/group/extpascal
License: BSD, http://www.opensource.org/licenses/bsd-license.php

0.9.8 Ext JS 3.2.1 support, 03-may-2010

- New ExtJS 3.2.1, 3.2.0, 3.1.1 and 3.1.0 support
- New ExtP Toolkit
- New Fusion Charts support
- New pre-built wrapper units for ExtJS, ExtToPascal is not necessary
- New methods AjaxExtFunction and AjaxForms
- New brMobileSafari browser type
- New ExtPascalSamples refactored
- New DebugExtJS define
- New WebServer property
- New StyleExtObject property in ExtComponent
- New JSDateToDateTime function
- New LineToPixels method
- New TExtThread.SetCSS method
- Updated Cachefly support
- Changed Codepress is now directly below httdocs
- Fixes on 64K limit, it is over
- Fixes for MS-IIS (DataStore.Load and FileUpload) 
- Fixes for Embedded WebServer (Ajax calls, FileUpload and Garbage Collector)
- Fixes for D2009/2010
- Fixes in ErrorMessages
- Fixes on FPC compatibility
- Fixes in CGIGateway, was removing spaces in the input
- Fixes in BeautifyJS
- Fixes in StrToJS
- Fixes in ExtToPascal
- Many other fixes

0.9.7, Draw2D initial support, 07-out-2009

- New Delphi 2010 support
- New Draw2D support
- New ExtJS 2.3.0 support
- New Windows 2003 server support
- New Upload file feature
- New Download file feature
- New TExtObject.JSString method
- New TExtObject.AjaxSelection method
- New TExtObject.MethodURI method
- New TExtThread.Charset property
- New ExtFixes? for ExtChart? support
- Updated ExtP Toolkit
- Enhanced DebugJS define for Firefox
- Fixes on Ajax multiple parameters
- Fixes on Self-Translation
- Fixes for Embedded WebServer
- Fixes for Windows Service mode with FPC
- More and enhanced error messages
- Other minor fixes

0.9.6, Ext JS 3.0.0 support, 13-jul-2009

0.9.5, Ext JS 2.2.1 support, 11-jul-2009, changes:

- Ext JS 2.2.1 support
- Pascal enumerated types support, see ExtFixes.txt for more informations
- PowerPC and Big-endian architectures support
- CacheFly option
- New helper functions SetMargins and SetPaddings
- Ext.ux.form.LovCombo support
- Ext.ux.grid.RecordForm support
- New SetIconCls method
- New properties: ImagePath, ExtBuild, Browser, 
- Improved TRegexp recognition
- Blocksocket fixes
- D2009 support
- New USESPUBLISHED symbol define
- Fixes for IIS
- Configuration and reconfigurantion process based on .INI file
- BeautifyJS/CSS functions for JavaScript debugging
- Embedded WebServer compiling fixed
- Embedded WebServer support for Linux (experimental)
- Fixed Issue 8:  "weird behavior of tab demo"
- Fixed Issue 14: "Ajax param can't be filled by many values which come from single control object"
- New parser to Ext JS 3

0.9.4, CodePress, 28-nov-2008, changes:

- New CodePress widget with Object Pascal syntax highlighting

0.9.3, Bug fix release, 21-nov-2008, changes:

- Fixed double free.

0.9.2, Beta 5 release, 13-nov-2008, changes:

- CHM and HTML help using Doc-O-Matic.
- Embedded WebServer option for Windows using Indy 10 by Vagner.
- Delphi Style event handlers by Vagner.
- Services support for Windows Vista by Patricio.
- Conversion from ISO-8859-1 to UTF8 upon Windows.
- On Linux sources should be UTF8.
- Fix for Response issue pointed by Rovi.
- Fixes for CGIGateway (loading forever issue) and BlockSocket on Linux/MacOS by Bee.
- New TApplication.Icon property.
- New TExtObject.JSExpression method.
- New TExtObject.Delete method.
- Updated TExtObject.Free method.
- New TFCGIThread.QueryAs methods.
- New TFCGIThread.BeforeThreadDestruction method.
- Removed some Ajax restrictions.
- Chars "_" and "|" are not used internally anymore.
- TExtThread.Language is a writeable property now.
- New ExtFixes classes and properties.
- A bunch of fixes and optimizations on Windows and Linux/MacOS platforms.
- Extensive tests (performance, memory use and reliability) upon Windows and Linux by Eneas.
- New performance Notes by Eneas.
- New SimplifiedFlow graph by Rovi
