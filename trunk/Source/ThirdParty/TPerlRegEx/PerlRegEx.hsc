HelpScribble project file.
16
WTfbsg-000110
112
1
TPerlRegEx Help


Copyright © 1999, 2005, 2008 Jan Goyvaerts
FALSE
0x0409
r:\JGsoft\HelpScribble\Images
1
BrowseButtons()
0
FALSE

FALSE
TRUE
16777215
0
16711680
8388736
255
FALSE
FALSE
FALSE
FALSE
150
50
600
500
TRUE
FALSE
1
FALSE
FALSE
Contents
%s Contents
Index
%s Index
Previous
Next
FALSE

122
1000
Scribble1000
PerlRegEx unit
PerlRegEx unit;PerlRegEx;DLL;PCRELIB.dll;NO_PCRE_DLL


perlregex:000010
Done



FALSE
33
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil\fcharset0 Arial;}{\f2\fnil Courier New;}{\f3\fnil\fcharset0 Courier New;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 PerlRegEx unit
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1005\}\cf4\{keepn\}\cf0\fs20 
\par \f1 TPerlRegEx is a Delphi VCL wrapper around the open source PCRE library, which implements Perl-Compatible Regular Expressions.  The supplied pcre3.dll contains PCRE 7.0, compiled with Unicode support.
\par 
\par For more information about PCRE, please visit \cf1\strike\f2 http://www.regular-expressions.info/pcre.html\cf3\strike0\{link=*! ExecFile("http://www.regular-expressions.info/pcre.html")\}\cf0\f0 
\par 
\par \f1 For more information about regular expressions in general, please visit \cf1\strike\f2 http://www.regular-expressions.info/\cf3\strike0\{link=*! ExecFile("http://www.regular-expressions.info/")\}
\par 
\par \cf0\f1 You can download the latest version of TPerlRegEx at \cf1\strike\f2 http://www.regular-expressions.info/\f3 delphi.html\cf3\strike0\f2\{link=*! ExecFile("http://www.regular-expressions.info/\f3 delphi.html\f2 ")\}\cf0\f0 
\par 
\par \f1 TPerlRegEx is licensed under the Mozilla Public License, version 1.1.\f0 
\par 
\par \f1 This help file only describes the TPerlRegEx component itself.
\par 
\par \b\f0\fs22 Components
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4 
\par 
\par \cf0\b\fs22 Classes
\par \cf2\b0\strike\fs20 TPerlRegExList\cf3\strike0\{linkID=1200\}\cf4 
\par 
\par \cf0\b\fs22 Types
\par \cf2\b0\strike\fs20 TPerlRegExOptions\cf3\strike0\{linkID=1300\}\cf4 
\par \cf2\strike TPerlRegExReplaceEvent\cf3\strike0\{linkID=1305\}\cf4 
\par \cf2\strike TPerlRegExState\cf3\strike0\{linkID=1310\}\cf4 
\par 
\par \cf0\b\fs22 Constants
\par \cf2\b0\strike\fs20 MAX_SUBEXPRESSIONS\cf3\strike0\{linkID=1500\}\cf4 
\par 
\par \cf0 
\par }
1005
Scribble1005
PerlRegEx unit - See also




Done



FALSE
6
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike TPerlRegEx component\cf3\strike0\{linkID=1010\}\cf0 
\par }
1010
Scribble1010
TPerlRegEx component
TPerlRegEx


perlregex:000020
Done


TPerlRegEx_Object;TPerlRegEx
FALSE
28
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil\fcharset0 Arial;}{\f2\fnil Courier New;}{\f3\fnil\fcharset2 Symbol;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 TPerlRegEx component
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul Properties\cf3\ulnone\{linkID=%1012\}\tab\cf2\ul Methods\cf3\ulnone\{linkID=%1013\}\tab\cf2\ul Events\cf3\ulnone\{linkID=%1014\}\cf4\{keepn\}\cf0\b\fs22 
\par Unit
\par \cf2\b0\strike\fs20 PerlRegEx\cf3\strike0\{linkID=1000\}\cf4 
\par 
\par \cf0\b\fs22 Description
\par \b0\fs20 The TPerlRegEx component aimes at providing any Delphi or C++Builder developer with the same, powerful regular expression capabilities provided by the Perl programming language, created by Larry Wall.\f1   It is implemented as a \cf2\strike wrapper around the open source PCRE library\cf3\strike0\{linkID=1000\}\cf0 .\f0 
\par 
\par \pard Note that this help file assumes that you know what regular expressions are and what you can do with them.  \f1 If you don't, you can find a very detailed tutorial about regular expressions at \cf1\strike\f2 http://www.regular-expressions.info/\cf3\strike0\{link=*! ExecFile("http://www.regular-expressions.info/")\}\cf0\f0 
\par 
\par \b\fs22 Tasks\b0\fs20 
\par \ul At design time:\ulnone 
\par \pard{\pntext\f3\'B7\tab}{\*\pn\pnlvlblt\pnf3\pnindent0{\pntxtb\'B7}}\fi-200\li200\tx200\f1 Drop a TPerlRegEx component on your form
\par {\pntext\f3\'B7\tab}Adjust the \cf2\strike Options property\cf3\strike0\{linkID=1045\}\cf0 
\par {\pntext\f3\'B7\tab}If you wish, enter a regular expression in the \cf2\strike RegEx property\cf3\strike0\{linkID=1050\}\cf0 .  You can also do this at run time.
\par {\pntext\f3\'B7\tab}If you want to do replacements, attach an \cf2\strike OnReplace event handler\cf3\strike0\{linkID=1180\}\cf0  or put a string in the \cf2\strike Replacement property\cf3\strike0\{linkID=1055\}\cf0 
\par {\pntext\f3\'B7\tab}\pard 
\par \ul At run time:\ulnone 
\par \pard{\pntext\f3\'B7\tab}{\*\pn\pnlvlblt\pnf3\pnindent0{\pntxtb\'B7}}\fi-200\li200\tx200 Assign a regular expression to the \cf2\strike RegEx property\cf3\strike0\{linkID=1050\}\cf0  if you did not do so at design time.
\par {\pntext\f3\'B7\tab}Assign a string to the \cf2\strike Subject property\cf3\strike0\{linkID=1100\}\cf0 
\par {\pntext\f3\'B7\tab}Call the \cf2\strike Match method\cf3\strike0\{linkID=1120\}\cf0 
\par {\pntext\f3\'B7\tab}If you assigned an \cf2\strike OnReplace event handler\cf3\strike0\{linkID=1180\}\cf0  or the \cf2\strike Replacement property\cf3\strike0\{linkID=1055\}\cf0  and Match returned true, call the \cf2\strike Replace method\cf3\strike0\{linkID=1135\}\cf0 
\par {\pntext\f3\'B7\tab}Call the \cf2\strike MatchAgain\cf3\strike0\{linkID=1125\}\cf0  method if Match returned true and you want to continue searching
\par {\pntext\f3\'B7\tab}Instead of calling \cf2\strike Match\cf3\strike0\{linkID=1120\}\cf0 , \cf2\strike MatchAgain\cf3\strike0\{linkID=1125\}\cf0  and \cf2\strike Replace\cf3\strike0\{linkID=1135\}\cf0 , call the \cf2\strike ReplaceAll method\cf3\strike0\{linkID=1140\}\cf0  to do a global substitution in the Subject string, like Perl's s///g operator.\f0 
\par {\pntext\f3\'B7\tab}}
1012
Scribble1012
TPerlRegEx - Properties




Done



FALSE
24
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red128\green0\blue0;\red0\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 Properties
\par \cf0\b0 
\par \cf2\{bmct runtime.bmp\}\cf3  Run-time only\tab\cf2\{bmct key.bmp\}\cf3  Key properties
\par \pard\tx200\tx640\cf2\{bmct runtime.bmp\}\tab\{bmct key.bmp\}\tab\cf4\strike Compiled\cf2\strike0\{linkID=1020\}\cf3 
\par \cf2\{bmct runtime.bmp\}\tab\{bmct key.bmp\}\tab\cf4\strike FoundMatch\cf2\strike0\{linkID=1025\}\cf3 
\par \cf2\{bmct runtime.bmp\}\tab\{bmct key.bmp\}\tab\cf4\strike MatchedExpression\cf2\strike0\{linkID=1030\}\cf3 
\par \cf2\{bmct runtime.bmp\}\tab\{bmct key.bmp\}\tab\cf4\strike MatchedExpressionLength\cf2\strike0\{linkID=1035\}\cf3 
\par \cf2\{bmct runtime.bmp\}\tab\{bmct key.bmp\}\tab\cf4\strike MatchedExpressionOffset\cf2\strike0\{linkID=1040\}\cf3 
\par \f1\tab\cf2\{bmct key.bmp\}\tab\cf4\strike Options\cf2\strike0\{linkID=1045\}\cf3 
\par \tab\cf2\{bmct key.bmp\}\tab\cf4\strike RegEx\cf2\strike0\{linkID=1050\}\cf3 
\par \tab\cf2\{bmct key.bmp\}\tab\cf4\strike Replacement\cf2\strike0\{linkID=1055\}\cf3 
\par \cf2\{bmct runtime.bmp\}\tab\{bmct key.bmp\}\tab\cf4\strike Start\cf2\strike0\{linkID=1060\}\cf3 
\par \cf2\{bmct runtime.bmp\}\tab\{bmct key.bmp\}\tab\cf4\strike State\cf2\strike0\{linkID=1065\}\cf3 
\par \cf2\{bmct runtime.bmp\}\tab\{bmct key.bmp\}\tab\cf4\strike Stop\cf2\strike0\{linkID=1070\}\cf3 
\par \cf2\{bmct runtime.bmp\}\tab\{bmct key.bmp\}\tab\cf4\strike Studied\cf2\strike0\{linkID=1075\}\cf3 
\par \cf2\{bmct runtime.bmp\}\tab\{bmct key.bmp\}\tab\cf4\strike SubExpressionCount\cf2\strike0\{linkID=1080\}\cf3 
\par \cf2\{bmct runtime.bmp\}\tab\{bmct key.bmp\}\tab\cf4\strike SubExpressionLengths\cf2\strike0\{linkID=1085\}\cf3 
\par \cf2\{bmct runtime.bmp\}\tab\{bmct key.bmp\}\tab\cf4\strike SubExpressionOffsets\cf2\strike0\{linkID=1090\}\cf3 
\par \cf2\{bmct runtime.bmp\}\tab\{bmct key.bmp\}\tab\cf4\strike SubExpressions\cf2\strike0\{linkID=1095\}\cf3 
\par \cf2\{bmct runtime.bmp\}\tab\{bmct key.bmp\}\tab\cf4\strike Subject\cf2\strike0\{linkID=1100\}\cf3 
\par \cf0\f0 
\par }
1013
Scribble1013
TPerlRegEx - Methods




Done



FALSE
18
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red128\green0\blue0;\red0\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 Methods
\par \cf0\b0 
\par \cf2\{bmct key.bmp\}\cf3  Key methods
\par \pard\tx200\tx640\f1\tab\cf2\{bmct key.bmp\}\tab\cf4\strike Compile\cf2\strike0\{linkID=1105\}\cf3 
\par \tab\cf2\{bmct key.bmp\}\tab\cf4\strike ComputeReplacement\cf2\strike0\{linkID=1110\}\cf3 
\par \tab\cf2\{bmct key.bmp\}\tab\cf4\strike EscapeRegExChars\cf2\strike0\{linkID=1115\}\cf3 
\par \tab\cf2\{bmct key.bmp\}\tab\cf4\strike Match\cf2\strike0\{linkID=1120\}\cf3 
\par \tab\cf2\{bmct key.bmp\}\tab\cf4\strike MatchAgain\cf2\strike0\{linkID=1125\}\cf3 
\par \tab\cf2\{bmct key.bmp\}\tab\cf4\strike NamedSubExpression\cf2\strike0\{linkID=1130\}\cf3 
\par \tab\cf2\{bmct key.bmp\}\tab\cf4\strike Replace\cf2\strike0\{linkID=1135\}\cf3 
\par \tab\cf2\{bmct key.bmp\}\tab\cf4\strike ReplaceAll\cf2\strike0\{linkID=1140\}\cf3 
\par \tab\cf2\{bmct key.bmp\}\tab\cf4\strike Split method\cf2\strike0\{linkID=1145\}\cf3 
\par \tab\cf2\{bmct key.bmp\}\tab\cf4\strike StoreSubExpressions\cf2\strike0\{linkID=1150\}\cf3 
\par \tab\cf2\{bmct key.bmp\}\tab\cf4\strike Study\cf2\strike0\{linkID=1155\}\cf3 
\par \cf0\f0 
\par }
1014
Scribble1014
TPerlRegEx - Events




Done



FALSE
9
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red128\green0\blue0;\red0\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 Events
\par \cf0\b0 
\par \cf2\{bmct key.bmp\}\cf3  Key events
\par \pard\tx200\tx640\f1\tab\cf2\{bmct key.bmp\}\tab\cf4\strike OnMatch\cf2\strike0\{linkID=1175\}\cf3 
\par \tab\cf2\{bmct key.bmp\}\tab\cf4\strike OnReplace\cf2\strike0\{linkID=1180\}\cf3 
\par \cf0\f0 
\par }
1020
Scribble1020
Compiled property
Compiled,TPerlRegEx;TPerlRegEx,Compiled


perlregex:000030
Done


TPerlRegEx_Compiled;Compiled_Property;Compiled
TRUE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 Compiled property
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1021\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4  component
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 property\b0  Compiled: Boolean;
\par \f0 
\par \b\fs22 Description
\par \b0\fs20 True if the \cf2\strike RegEx\cf3\strike0\{linkID=1050\}\cf0  has already been compiled\f2  by calling \cf2\strike Compile\cf3\strike0\{linkID=1105\}\cf0 .
\par \f0 Run-time and read-only\f2 .\f0 
\par 
\par }
1021
Scribble1021
Compiled property - See also




Done



FALSE
6
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike Compile method\cf3\strike0\{linkID=1105\}\cf0 
\par }
1025
Scribble1025
FoundMatch property
FoundMatch,TPerlRegEx;TPerlRegEx,FoundMatch


perlregex:000040
Done


TPerlRegEx_FoundMatch;FoundMatch_Property;FoundMatch
FALSE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 FoundMatch property
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1026\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4  component
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 property\b0  FoundMatch: Boolean;
\par \f0 
\par \b\fs22 Description
\par \b0\fs20 Returns True when \cf2\strike MatchedExpression\cf3\strike0\{linkID=1030\}\cf0  and \cf2\strike SubExpression\f2 s\cf3\strike0\{linkID=1095\}\cf0\f0  indicate a match\f2 .\f0 
\par Run-time and read-only\f2 .\f0 
\par 
\par }
1026
Scribble1026
FoundMatch property - See also




Done



FALSE
10
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike MatchedExpression property\cf3\strike0\{linkID=1030\}\cf0 
\par \cf2\strike SubExpressions property\cf3\strike0\{linkID=1095\}
\par \cf2\strike 
\par Match method\cf3\strike0\{linkID=1120\}
\par \cf2\strike MatchAgain method\cf3\strike0\{linkID=1125\}\cf0 
\par }
1030
Scribble1030
MatchedExpression property
MatchedExpression,TPerlRegEx;TPerlRegEx,MatchedExpression


perlregex:000050
Done


TPerlRegEx_MatchedExpression;MatchedExpression_Property;MatchedExpression
FALSE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 MatchedExpression property
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1031\}\tab\cf2\strike Example\cf3\strike0\{linkID=%1032\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4  component
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 property\b0  MatchedExpression: \b string\b0 ;
\par \f0 
\par \b\fs22 Description
\par \b0\f2\fs20 The substring matched by the regular expression since the last call to the \cf2\strike Match method\cf3\strike0\{linkID=1120\}\cf0  or the \cf2\strike MatchAgain method\cf3\strike0\{linkID=1125\}\cf0 .\f0 
\par Run-time and read-only\f2 .\f0 
\par 
\par }
1031
Scribble1031
MatchedExpression property - See also




Done



FALSE
13
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike FoundMatch property\cf3\strike0\{linkID=1025\}\cf0 
\par \cf2\strike MatchedExpressionLength property\cf3\strike0\{linkID=1035\}\cf0 
\par \cf2\strike MatchedExpressionOffset property\cf3\strike0\{linkID=1040\}
\par \cf2\strike SubExpressions property\cf3\strike0\{linkID=1095\}
\par \cf0 
\par \cf2\strike Subject property\cf3\strike0\{linkID=1100\}\cf0 
\par \cf2\strike Match method\cf3\strike0\{linkID=1120\}\cf0 
\par \cf2\strike MatchAgain method\cf3\strike0\{linkID=1125\}\cf0 
\par }
1032
Scribble1032
MatchedExpression property - Example




Done



FALSE
11
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fswiss Arial;}{\f2\fmodern Courier New;}}
{\colortbl ;\red0\green0\blue255;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 MatchedExpression property example
\par \cf0\b0 
\par \pard\tx1435\tx2875\tx4315\tx5755\tx7195\tx8635\f1 This example assumes you placed a TLabel and a TPerlRegEx on a form, and that PerlRegEx.Subject is set to a meaningful test string.
\par 
\par \pard\b\f2 with\b0  PerlRegEx1 \b do\b0  \b begin\b0 
\par   RegEx := 'Windows|Linux';  \i // Matches 'Windows' or 'Linux', whichever comes first\i0 
\par   \b if\b0  Match \b then\b0  Label1.Caption := MatchedExpression + ' rulez!';
\par \b end\b0 ;\f0 
\par }
1035
Scribble1035
MatchedExpressionLength property
MatchedExpressionLength,TPerlRegEx;TPerlRegEx,MatchedExpressionLength


perlregex:000060
Done


TPerlRegEx_MatchedExpressionLength;MatchedExpressionLength_Property;MatchedExpressionLength
FALSE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fswiss Arial;}{\f3\fmodern Courier New;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 MatchedExpressionLength property
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1036\}\tab\cf2\strike Example\cf3\strike0\{linkID=%1037\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4  component
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 property\b0  MatchedExpressionLength: Integer;
\par \f0 
\par \b\fs22 Description
\par \pard\b0\f2\fs20 Length of the substring matched by the regular expression since the last call to \cf2\strike Match method\cf3\strike0\{linkID=1120\}\cf0  or \cf2\strike MatchAgain method\cf3\strike0\{linkID=1125\}\cf0 .  This is a shortcut to \f3 Length(MatchedExpression)\f2  and will execute much faster.
\par \pard\tx1435\tx2875\tx4315\tx5755\tx7195\tx8635 Run-time and read-only.
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\f0 
\par }
1036
Scribble1036
MatchedExpressionLength property - See also




Done



FALSE
14
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike FoundMatch property\cf3\strike0\{linkID=1025\}\cf0 
\par \cf2\strike MatchedExpression property\cf3\strike0\{linkID=1030\}\cf0 
\par \cf2\strike MatchedExpressionOffset property\cf3\strike0\{linkID=1040\}
\par \cf2\strike SubExpressions property\cf3\strike0\{linkID=1095\}
\par \cf2\strike SubExpressionLengths property\cf3\strike0\{linkID=1085\}
\par \cf0 
\par \cf2\strike Subject property\cf3\strike0\{linkID=1100\}\cf0 
\par \cf2\strike Match method\cf3\strike0\{linkID=1120\}\cf0 
\par \cf2\strike MatchAgain method\cf3\strike0\{linkID=1125\}\cf0 
\par }
1037
Scribble1037
MatchedExpressionLength property - Example




Done



FALSE
15
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fswiss Arial;}{\f2\fmodern Courier New;}}
{\colortbl ;\red0\green0\blue255;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 MatchedExpressionLength property example
\par \cf0\b0 
\par \pard\tx1435\tx2875\tx4315\tx5755\tx7195\tx8635\f1 This example assumes you placed a TLabel and a TPerlRegEx on a form.
\par 
\par \pard\b\f2 with\b0  PerlRegEx1 \b do\b0  \b begin\b0 
\par   Subject := 'Rating: AAA';
\par   RegEx := 'A+';  \i // Matches the first occurence of 'A', including any A's that may follow right after it\i0 
\par   \b if\b0  Match \b then\b0  Label1.Caption := 
\par     'You have been rated with ' + IntToStr(MatchedExpressionLength) + 'A''s'
\par   \b else\b0 
\par     Label1.Caption := 'You have a poor rating';
\par \b end\b0 ;\f0 
\par }
1040
Scribble1040
MatchedExpressionOffset property
MatchedExpressionOffset,TPerlRegEx;TPerlRegEx,MatchedExpressionOffset


perlregex:000070
Done


TPerlRegEx_MatchedExpressionOffset;MatchedExpressionOffset_Property;MatchedExpressionOffset
FALSE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 MatchedExpressionOffset property
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1041\}\tab\cf2\strike Example\cf3\strike0\{linkID=%1042\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4  component
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 property\b0  MatchedExpressionOffset: Integer;
\par \f0 
\par \b\fs22 Description
\par \b0\fs20 Character offset in the \cf2\strike Subject string\cf3\strike0\{linkID=1100\}\cf0  at which the \cf2\strike matched substring\cf3\strike0\{linkID=1030\}\cf0  starts\f2 .\f0 
\par Run-time and read-only.
\par 
\par }
1041
Scribble1041
MatchedExpressionOffset property - See also




Done



FALSE
14
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike FoundMatch property\cf3\strike0\{linkID=1025\}\cf0 
\par \cf2\strike MatchedExpression property\cf3\strike0\{linkID=1030\}\cf0 
\par \cf2\strike MatchedExpressionLength property\cf3\strike0\{linkID=1035\}\cf2\strike 
\par SubExpressions property\cf3\strike0\{linkID=1095\}
\par \cf2\strike SubExpressionOffsets property\cf3\strike0\{linkID=1090\}
\par \cf0 
\par \cf2\strike Subject property\cf3\strike0\{linkID=1100\}\cf0 
\par \cf2\strike Match method\cf3\strike0\{linkID=1120\}\cf0 
\par \cf2\strike MatchAgain method\cf3\strike0\{linkID=1125\}\cf0 
\par }
1042
Scribble1042
MatchedExpressionOffset property - Example




Done



FALSE
12
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fswiss Arial;}{\f2\fmodern Courier New;}}
{\colortbl ;\red0\green0\blue255;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 MatchedExpressionOffset property example
\par \cf0\b0 
\par \pard\tx1435\tx2875\tx4315\tx5755\tx7195\tx8635\f1 This example assumes you placed a TLabel and a TPerlRegEx on a form, and that PerlRegEx.Subject is set to a meaningful test string.
\par 
\par \pard\b\f2 with\b0  PerlRegEx1 \b do\b0  \b begin\b0 
\par   RegEx := 'Bye';
\par   \b if\b0  Match \b then\b0  Label1.Caption := 'Before typing "Bye", you typed: ' +
\par     Copy(Subject, 1, MatchedExpressionOffset-1);
\par \b end\b0 ;\f0 
\par }
1045
Scribble1045
Options property
Options,TPerlRegEx;TPerlRegEx,Options


perlregex:000080
Done


TPerlRegEx_Options;Options_Property;Options
FALSE
23
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fswiss Arial;}{\f3\fswiss\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 Options property
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1046\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4  component
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 property\b0  Options: TPerlRegExOptions;
\par \f0 
\par \b\fs22 Description
\par \pard\tx1435\tx2875\tx4315\tx5755\tx7195\tx8635\b0\f2\fs20 These are the most important options you can specify:
\par 
\par \pard\fi-1415\li1415\tx1435\tx2875\tx4315\tx5755\tx7195\tx8635\b preCaseLess\b0\tab Tries to match the regex without paying attention to case.  If set, 'Bye' will match 'Bye', 'bye', 'BYE' and even 'byE', 'bYe', etc.  Otherwise, only 'Bye' will be matched.  Equivalent to Perl's /i modifier.
\par \b preMultiLine\b0\tab The ^ (beginning of string) and $ (ending of string) regex operaters will also match right after and right before a newline in the Subject string.  This effectively treats one string with multiple lines as multiple strings.  Equivalent to Perl's /m modifier.
\par \b preSingleLine\b0  Normally, dot (.) matches anything but a newline (\\n).  With preSingleLine, dot (.) will match anything, including newlines.  This allows a multiline string to be regarded as a single entity.  Equivalent to Perl's /s modifier.
\par \tab Note that preMultiLine and preSingleLine can be used together.
\par \b preExtended\b0\tab\f3 Allow regex to contain extra whitespace, newlines and Perl-style comments, all of which will be filtered out.  This is sometimes called "free-spacing mode".\f2 
\par \b pre\f3 Anchore\f2 d\b0\tab\f3 Allows the regex to match only at the start of the subject or right after the previous match.\f2 
\par \b preUngreedy\b0\tab Repeat operaters (?, *, +, \{\-num,num\}) are greedy by default, i.e. they try to match as many characters as possible.  Set preUngreedy to use ungreedy repeat operators by default, i.e. they try to match as few characters as possible.
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\f0 
\par }
1046
Scribble1046
Options property - See also




Done



FALSE
7
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike State property\cf3\strike0\{linkID=1065\}
\par \cf2\strike TPerlRegExOptions type\cf3\strike0\{linkID=1300\}\cf0 
\par }
1050
Scribble1050
RegEx property
RegEx,TPerlRegEx;TPerlRegEx,RegEx


perlregex:000090
Done


TPerlRegEx_RegEx;RegEx_Property;RegEx
FALSE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 RegEx property
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1051\}\tab\cf2\strike Example\cf3\strike0\{linkID=%1052\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4  component
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 property\b0  RegEx: \b string\b0 ;
\par \f0 
\par \b\fs22 Description
\par \b0\fs20 The regular expression to be matched\f2 .  See \cf1\strike\f1 http://www.regular-expressions.info/\cf3\strike0\{link=*! ExecFile("http://www.regular-expressions.info/")\}\cf0\f2  to learn how to write a regular expression.
\par 
\par The TPerlRegEx component uses the "PCRE" regular expression flavor explained at \cf1\strike\f1 http://www.regular-expressions.info/refflavors.html\cf3\strike0\{link=*! ExecFile("http://www.regular-expressions.info/refflavors.html")\}\cf0\f2 .\f0 
\par }
1051
Scribble1051
RegEx property - See also




Done



FALSE
7
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike Subject property\cf3\strike0\{linkID=1100\}\cf0 
\par \cf2\strike Match method\cf3\strike0\{linkID=1120\}\cf0 
\par }
1052
Scribble1052
RegEx property - Example




Done



FALSE
10
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fmodern Courier New;}}
{\colortbl ;\red0\green0\blue255;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 RegEx property example
\par \cf0\b0 
\par \b\f1 with\b0  PerlRegEx1 \b do\b0  \b begin\b0 
\par   RegEx := 'Perl';
\par   Subject := 'Another example for TPerlRegEx';
\par   \b if\b0  Match \b then\b0  ShowMessage('We found another Perl!');
\par \b end\b0 ;\f0 
\par }
1055
Scribble1055
Replacement property
Replacement,TPerlRegEx;TPerlRegEx,Replacement


perlregex:000100
Done


TPerlRegEx_Replacement;Replacement_Property;Replacement
FALSE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 Replacement property
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1056\}\tab\cf2\strike Example\cf3\strike0\{linkID=%1057\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4  component
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 property\b0  Replacement: \b string\b0 ;
\par \f0 
\par \b\fs22 Description
\par \b0\fs20 String to replace matched expression with. \f2 B\f0 ackreferences \f2\\0, \\1, \\2 etc. as well as $1, $2, $3, etc. \f0 will be substituted with \cf2\strike SubExpressions\f2 [number]\cf3\strike0\{linkID=1095\}\cf0 .\f0 
\par 
\par \f2 The TPerlRegEx component uses the "JGsoft" replacement text flavor explained at \cf1\strike\f1 http://www.regular-expressions.info/refreplace.html\cf3\strike0\{link=*! ExecFile("http://www.regular-expressions.info/refreplace.html")\}\cf0\f2 .\f0 
\par }
1056
Scribble1056
Replacement property - See also




Done



FALSE
11
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike RegEx property\cf3\strike0\{linkID=1050\}\cf0 
\par \cf2\strike Subject property\cf3\strike0\{linkID=1100\}\cf0 
\par \cf2\strike SubExpressions property\cf3\strike0\{linkID=1095\}\cf0 
\par \cf2\strike Replace method\cf3\strike0\{linkID=1135\}\cf0 
\par \cf2\strike ReplaceAll method\cf3\strike0\{linkID=1140\}\cf0 
\par \cf2\strike OnReplace event\cf3\strike0\{linkID=1180\}\cf0 
\par }
1057
Scribble1057
Replacement property - Example




Done



FALSE
14
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fswiss Arial;}{\f2\fmodern Courier New;}}
{\colortbl ;\red0\green0\blue255;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 Replacement property example
\par \cf0\b0 
\par \pard\tx1435\tx2875\tx4315\tx5755\tx7195\tx8635\f1 This example will show a messagebox saying "Foo is the name of the bar I like".
\par 
\par \pard\b\f2 with\b0  PerlRegEx1 \b do\b0  \b begin\b0 
\par   Subject := 'I like to hang out at Foo bar';
\par   RegEx := '([A-Za-z]+) bar';
\par   Replacement := '\\1 is the name of the bar I like';
\par   \b if\b0  Match \b then\b0  Replace;
\par   ShowMessage(RegEx.Subject);
\par \b end\b0 ;\f0 
\par }
1060
Scribble1060
Start property
Start,TPerlRegEx;TPerlRegEx,Start


perlregex:000110
Done


TPerlRegEx_Start;Start_Property;Start
FALSE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 Start property
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1061\}\tab\cf2\strike Example\cf3\strike0\{linkID=%1072\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4  component
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 property\b0  Start: Integer;
\par \f0 
\par \b\fs22 Description
\par \b0\fs20 Starting position in \cf2\strike Subject\cf3\strike0\{linkID=1100\}\cf0  from which \cf2\strike MatchAgain\cf3\strike0\{linkID=1125\}\cf0  begins\f2 .
\par 
\par By default, MatchAgain continues from the end of the previous match, or from the start of the subject if there is no previous match.  Set the Start property to continue searching from another position.\f0 
\par }
1061
Scribble1061
Start property - See also




Done



FALSE
8
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike Stop property\cf3\strike0\{linkID=1070\}
\par \cf2\strike Subject property\cf3\strike0\{linkID=1100\}
\par \cf2\strike MatchAgain method\cf3\strike0\{linkID=1125\}\cf0 
\par }
1065
Scribble1065
State property
State,TPerlRegEx;TPerlRegEx,State


perlregex:000120
Done


TPerlRegEx_State;State_Property;State
FALSE
21
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}{\f3\fswiss Arial;}{\f4\fswiss\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 State property
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1066\}\tab\cf2\strike Example\cf3\strike0\{linkID=%1067\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4  component
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 property\b0  State: TPerlRegExState;
\par \f0 
\par \b\fs22 Description
\par \b0\fs20 State of \f2 the \cf2\strike\f0 Subject\f2  string\cf3\strike0\{linkID=1100\}\cf0 .\f0 
\par 
\par \pard\fi-1415\li1415\tx1435\tx2875\tx4315\tx5755\tx7195\tx8635\b\f3 preNot\f4 BOL\b0\f3\tab\f4 Do not match ^ or \\A at the start of the subject string.  Set this state if the subject string is not the logical beginning of the data the user is working with.
\par 
\par \b\f3 preNot\f4 EOL\b0\f3\tab\f4 Do not match $ or \\Z at the end of the subject string.  Set this state if the subject string is not the logical end of the data the user is working with.\f3 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\f0 
\par \pard\fi-1415\li1415\tx1435\tx2875\tx4315\tx5755\tx7195\tx8635\b\f3 preNotEmpty\b0\tab Do not match an empty string, i.e. if \cf2\strike Match\cf3\strike0\{linkID=1120\}\cf0  returns true, \cf2\strike MatchedExpression\cf3\strike0\{linkID=1030\}\cf0  will never be an empty string.  This can be useful if everything in your regular expression is optional (e.g.: a?b?c?).  The match returned is normally the leftmost, longest match.  If the subject is "dabdabc", it would match at the empty string right before "d".  With this option, the regex will match "ab" (chars 2 and 3 in the subject), the leftmost, longest match that is not an empty string.  \cf2\strike MatchAgain\cf3\strike0\{linkID=1125\}\cf0  will match "abc" at the end of the subject.
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\f0 
\par }
1066
Scribble1066
State property - See also




Done



FALSE
8
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike Subject property\cf3\strike0\{linkID=1100\}\cf0 
\par \cf2\strike Options property\cf3\strike0\{linkID=1045\}
\par \cf2\strike TPerlRegExState type\cf3\strike0\{linkID=1310\}\cf0 
\par }
1067
Scribble1067
State property - Example




Done



FALSE
10
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil\fcharset0 Arial;}{\f2\fnil Courier New;}}
{\colortbl ;\red0\green0\blue255;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 State property example
\par \cf0\b0 
\par \f1 You can implement a "find from cursor" function as follows:
\par 
\par \f2 PerlRegEx1.Subject := Data.GetString(CursorPosition, Data.DataLength);
\par PerlRegEx1.State := [preNotBOL];
\par PerlRegEx1.Match;\f0 
\par }
1070
Scribble1070
Stop property
Stop,TPerlRegEx;TPerlRegEx,Stop


perlregex:000130
Done


TPerlRegEx_Stop;Stop_Property;Stop
FALSE
15
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 Stop property
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1071\}\tab\cf2\strike Example\cf3\strike0\{linkID=%1072\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4  component
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 property\b0  Stop: Integer;
\par \f0 
\par \b\fs22 Description
\par \b0\fs20 Last character in Subject that \cf2\strike Match\cf3\strike0\{linkID=1120\}\cf0  and \cf2\strike MatchAgain\cf3\strike0\{linkID=1125\}\cf0  search through\f2 .  By default, they search until the end of the string.  Use the Stop property to search through only part of the string without having to reallocate a truncated string.\f0 
\par 
\par }
1071
Scribble1071
Stop property - See also




Done



FALSE
8
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike Start property\cf3\strike0\{linkID=1060\}
\par \cf2\strike Subject property\cf3\strike0\{linkID=1100\}
\par \cf2\strike MatchAgain method\cf3\strike0\{linkID=1125\}\cf0 
\par }
1072
Scribble1072
Stop property - Example




Done



FALSE
18
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil\fcharset0 Arial;}{\f2\fnil Courier New;}}
{\colortbl ;\red0\green0\blue255;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 Stop property example
\par \cf0\b0 
\par \f1 Search through characters 100..200 in S this way:
\par 
\par \f2 PerlRegEx1.Subject := S;
\par PerlRegEx1.Start := 100;
\par PerlRegEx1.Stop := 200;
\par PerlRegEx1.MatchAgain;\f1 
\par 
\par This is faster than:
\par 
\par \f2 PerlRegEx1.Subject := Copy(S, 100, 100);
\par PerlRegEx1.MatchAgain;\f1 
\par \f0 
\par \f1 since the former method does not allocate any memory to keep a duplicate of S.\f0 
\par }
1075
Scribble1075
Studied property
Studied,TPerlRegEx;TPerlRegEx,Studied


perlregex:000140
Done


TPerlRegEx_Studied;Studied_Property;Studied
FALSE
17
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fswiss Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 Studied property
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1076\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4  component
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 property\b0  Studied: Boolean;
\par \f0 
\par \b\fs22 Description
\par \pard\b0\f2\fs20 Run-time and read-only.
\par True if the RegEx has already been studied by the \cf2\strike Study method\cf3\strike0\{linkID=1155\}\cf0 .
\par Calling Study when Studied is already True, has no effect.
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\f0 
\par }
1076
Scribble1076
Studied property - See also




Done



FALSE
8
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike RegEx property\cf3\strike0\{linkID=1050\}\cf0 
\par \cf2\strike Compile method\cf3\strike0\{linkID=1105\}\cf0 
\par \cf2\strike Study method\cf3\strike0\{linkID=1155\}\cf0 
\par }
1080
Scribble1080
SubExpressionCount property
SubExpressionCount,TPerlRegEx;TPerlRegEx,SubExpressionCount


perlregex:000150
Done


TPerlRegEx_SubExpressionCount;SubExpressionCount_Property;SubExpressionCount
FALSE
18
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fswiss Arial;}{\f3\fswiss\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 SubExpressionCount property
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1081\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4  component
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 property\b0  SubExpressionCount: Integer;
\par \f0 
\par \b\fs22 Description
\par \pard\b0\f2\fs20 Run-time and read-only.
\par Number of matched subexpressions stored in the \cf2\strike SubExpressions\cf3\strike0\{linkID=1095\}\cf0  array.\f3   This number is the number of the highest-numbered capturing group in your regular expression that actually participated in the last match.  It may be less than the number of capturing groups in your regular expression.
\par 
\par E.g. when the regex "(a)|(b)" matches "a", SubExpressionCount will be 1.  When the same regex matches "b", SubExpressionCount will be 2.\f2 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\f0 
\par }
1081
Scribble1081
SubExpressionCount property - See also




Done



FALSE
8
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike SubExpressionOffsets property\cf3\strike0\{linkID=1090\}\cf0 
\par \cf2\strike SubExpressions property\cf3\strike0\{linkID=1095\}\cf0 
\par 
\par }
1085
Scribble1085
SubExpressionLengths property
SubExpressionLengths,TPerlRegEx;TPerlRegEx,SubExpressionLengths


perlregex:000160
Done


TPerlRegEx_SubExpressionLengths;SubExpressionLengths_Property;SubExpressionLengths
FALSE
17
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fswiss Arial;}{\f3\fmodern Courier New;}{\f4\fswiss\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 SubExpressionLengths property
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1086\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4  component
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 property\b0  SubExpressionLengths[Index: Integer: Integer;
\par \f0 
\par \b\fs22 Description
\par \pard\b0\f2\fs20 Run-time and read-only. 
\par Lengths of the subexpression strings in the \cf2\strike SubExpressions\cf3\strike0\{linkID=1095\}\cf0  array.
\par Querying \f3 SubExpressionLengths[Index]\f2  is faster than Length(SubExpressions[Index]), since the former only needs to do a few integer computations, while the latter actually extracts a string from the \cf2\strike Subjec\f4 t\cf3\strike0\{linkID=1100\}\cf0\f2 .
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640 Valid range for Index is 0..\cf2\strike SubExpressionCount\cf3\strike0\{linkID=1080\}\cf0 .\f0 
\par }
1086
Scribble1086
SubExpressionLengths property - See also




Done



FALSE
8
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike SubExpressionCount property\cf3\strike0\{linkID=1080\}\cf0 
\par \cf2\strike SubExpressionOffsets property\cf3\strike0\{linkID=1090\}\cf0 
\par \cf2\strike SubExpressions property\cf3\strike0\{linkID=1095\}\cf0 
\par }
1090
Scribble1090
SubExpressionOffsets property
SubExpressionOffsets,TPerlRegEx;TPerlRegEx,SubExpressionOffsets


perlregex:000170
Done


TPerlRegEx_SubExpressionOffsets;SubExpressionOffsets_Property;SubExpressionOffsets
FALSE
18
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fswiss Arial;}{\f3\fmodern Courier New;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 SubExpressionOffsets property
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1091\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4  component
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 property\b0  SubExpressionOffsets[Index: Integer: Integer;
\par \f0 
\par \b\fs22 Description
\par \pard\b0\f2\fs20 Run-time and read-only.
\par Character offsets in the Subject string of the \cf2\strike SubExpressions\cf3\strike0\{linkID=1095\}\cf0  strings.
\par Valid range for Index is 0..\cf2\strike SubExpressionCount\cf3\strike0\{linkID=1080\}\cf0 .
\par 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\f3 SubExpressions[Index] = Copy(Subject, SubExpressionOffsets[Index], SubExpressionLengths[Index]);\f0 
\par }
1091
Scribble1091
SubExpressionOffsets property - See also




Done



FALSE
8
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike SubExpressionCount property\cf3\strike0\{linkID=1080\}\cf0 
\par \cf2\strike SubExpressionLengths property\cf3\strike0\{linkID=1085\}\cf0 
\par \cf2\strike SubExpressions property\cf3\strike0\{linkID=1095\}\cf0 
\par }
1095
Scribble1095
SubExpressions property
SubExpressions,TPerlRegEx;TPerlRegEx,SubExpressions


perlregex:000180
Done


TPerlRegEx_SubExpressions;SubExpressions_Property;SubExpressions
FALSE
21
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fswiss Arial;}{\f3\fmodern Courier New;}{\f4\fmodern\fcharset0 Courier New;}{\f5\fswiss\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 SubExpressions property
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1096\}\tab\cf2\strike Example\cf3\strike0\{linkID=%1097\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4  component
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 property\b0  SubExpressions[Index: Integer: \b string\b0 ;
\par \f0 
\par \b\fs22 Description
\par \pard\b0\f2\fs20 Run-time and read-only. 
\par Matched subexpressions after a regex has been matched.
\par \f3 SubExpressions[0] = \strike MatchedExpression\strike0\{linkID=10\f4 30\f3\}\f2 
\par The item count in the SubExpressions array increases with one for every opening bracket in the regex.  The text matched by the part of the regex between that opening bracket and its corresponding closing bracket, will be put in the SubExpressions array.
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640 
\par Valid range for Index is 0..\cf2\strike SubExpressionCoun\f5 t\cf3\strike0\{linkID=1080\}\cf0\f2 .  Note that SubExpressionCount need not be equal to the number of opening brackets, since it may happen that only part of the regular expression played a role in the current match.
\par 
\par \f5 If you used named capturing groups in the regular expressions, use the \cf2\strike NamedSubExpression method\cf3\strike0\{linkID=1130\}\cf0  to retrieve the index number of a named group.\f0 
\par }
1096
Scribble1096
SubExpressions property - See also




Done



FALSE
12
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike MatchedExpression property\cf3\strike0\{linkID=1030\}\cf0 
\par \cf2\strike SubExpressionCount property\cf3\strike0\{linkID=1080\}\cf0 
\par \cf2\strike SubExpressionLengths property\cf3\strike0\{linkID=1085\}\cf0 
\par \cf2\strike SubExpressionOffsets property\cf3\strike0\{linkID=1090\}\cf0 
\par \cf2\strike Match method\cf3\strike0\{linkID=1255\}
\par \cf2\strike NamedSubExpression method\cf3\strike0\{linkID=1130\}
\par \cf2\strike StoreSubExpressions method\cf3\strike0\{linkID=1150\}\cf0 
\par }
1097
Scribble1097
SubExpressions property - Example




Done



FALSE
14
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fmodern Courier New;}}
{\colortbl ;\red0\green0\blue255;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 SubExpressions property example
\par \cf0\b0 
\par \b\f1 with\b0  PerlRegEx1 \b do\b0  \b begin\b0 
\par   RegEx := '(Joe|Jack|William|Avarell) (Dalton)? did it';
\par   Subject := 'The sheriff told us that Joe did it.';
\par   \b if\b0  Match \b then\b0 
\par     \b if\b0  SubExpressionCount = 2 \b then\b0 
\par       ShowMessage('Wanted: ' + SubExpressions[1] + ' ' + SubExpressions[2])
\par     \b else\b0 
\par       ShowMessage('Wanted: ' + SubExpressions[1] + ' (probably Dalton)');
\par \b end\b0 ;\f0 
\par }
1100
Scribble1100
Subject property
Subject,TPerlRegEx;TPerlRegEx,Subject


perlregex:000190
Done


TPerlRegEx_Subject;Subject_Property;Subject
FALSE
15
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 Subject property
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1101\}\tab\cf2\strike Example\cf3\strike0\{linkID=%1102\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4  component
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 property\b0  Subject: \b string\b0 ;
\par \f0 
\par \b\fs22 Description
\par \b0\fs20 The string on which \cf2\strike Match\cf3\strike0\{linkID=1120\}\cf0  will try to match \cf2\strike RegEx\cf3\strike0\{linkID=1050\}\cf0\f2 .\f0 
\par Run-time only\f2 .\f0 
\par }
1101
Scribble1101
Subject property - See also




Done



FALSE
7
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike RegEx property\cf3\strike0\{linkID=1050\}\cf0 
\par \cf2\strike Match method\cf3\strike0\{linkID=1120\}\cf0 
\par }
1102
Scribble1102
Subject property - Example




Done



FALSE
11
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fmodern Courier New;}}
{\colortbl ;\red0\green0\blue255;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 Subject property example
\par \cf0\b0 
\par \b\f1 with\b0  PerlRegEx1 \b do\b0  \b begin\b0 
\par   RegEx := Edit1.Text;
\par   Subject := Edit2.Text;
\par   \b if\b0  Match \b then\b0  ShowMessage('The regex you entered in Edit1 matches the string in Edit2')
\par     \b else\b0  ShowMessage('The regex you entered in Edit1 does not match the string in Edit2');
\par \b end\b0 ;\f0 
\par }
1105
Scribble1105
Compile method
Compile,TPerlRegEx;TPerlRegEx,Compile


perlregex:000200
Done


TPerlRegEx_Compile;Compile_Method;Compile
TRUE
15
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fswiss Arial;}{\f3\fswiss\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 Compile method
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1106\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4  component
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 class\b0  \b procedure\b0  Compile;
\par \f0 
\par \b\fs22 Description
\par \pard\b0\f2\fs20 Before it can be used, the regular expression needs to be compiled.  \cf2\strike Match\cf3\strike0\{linkID=1120\}\cf0  will call Compile automatically if you did not do so.
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640 If the regular expression will be applied in time-critical code, you may wish to compile it during your application's initialization.\f3   You may also want to call \cf2\strike Study\cf3\strike0\{linkID=1155\}\cf0  to further optimize the execution of the regex.\f0 
\par }
1106
Scribble1106
Compile method - See also




Done



FALSE
8
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fswiss Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike\f1 Compiled property\cf3\strike0\{linkID=1020\}\cf2 
\par \strike Match method\cf3\strike0\{linkID=1120\}\cf2 
\par \strike Study method\cf3\strike0\{linkID=1155\}\cf0\f0 
\par }
1110
Scribble1110
ComputeReplacement method
ComputeReplacement,TPerlRegEx;TPerlRegEx,ComputeReplacement


perlregex:000210
Done


TPerlRegEx_ComputeReplacement;ComputeReplacement_Method;ComputeReplacement
FALSE
14
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 ComputeReplacement method
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1111\}\tab\cf2\strike Example\cf3\strike0\{linkID=%1112\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4  component
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 function\b0  ComputeReplacement: \b string\b0 ;
\par \f0 
\par \b\fs22 Description
\par \b0\fs20 Returns \cf2\strike Replacement\cf3\strike0\{linkID=1055\}\cf0  with \cf2\strike backreferences\cf3\strike0\{linkID=1095\}\cf0\f2  found by the last \cf2\strike match\cf3\strike0\{linkID=1120\}\cf0\f0  filled in\f2 .  Unlike the \cf2\strike Replace method\cf3\strike0\{linkID=1135\}\cf0 , ComputeReplacement does not modify \cf2\strike Subject\cf3\strike0\{linkID=1100\}\cf0 .\f0 
\par }
1111
Scribble1111
ComputeReplacement method - See also




Done



FALSE
7
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike Replacement property\cf3\strike0\{linkID=1055\}\cf0 
\par \cf2\strike Replace method\cf3\strike0\{linkID=1135\}\cf0 
\par }
1112
Scribble1112
ComputeReplacement method - Example




Done



FALSE
13
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fswiss Arial;}{\f2\fmodern Courier New;}{\f3\fmodern\fcharset0 Courier New;}}
{\colortbl ;\red0\green0\blue255;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 ComputeReplacement method example
\par \cf0\b0 
\par \pard\tx1435\tx2875\tx4315\tx5755\tx7195\tx8635\f1 This example will show a messagebox saying "Foo is the name of the bar I like".
\par 
\par \pard\b\f2 with\b0  PerlRegEx1 \b do\b0  \b begin\b0 
\par   Subject := 'I like to hang out at Foo bar';
\par   RegEx := '([A-Za-z]+) bar';
\par   Replacement := '\\1 is the name of the bar I like';
\par   \b if\b0  Match \b then\b0  ShowMessage(RegEx.\f3 ComputeReplacement\f2 );
\par \b end\b0 ;\f0 
\par }
1115
Scribble1115
EscapeRegExChars method
EscapeRegExChars,TPerlRegEx;TPerlRegEx,EscapeRegExChars


perlregex:000220
Done


TPerlRegEx_EscapeRegExChars;EscapeRegExChars_Method;EscapeRegExChars
FALSE
14
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 EscapeRegExChars method
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1116\}\tab\cf2\strike Example\cf3\strike0\{linkID=%1117\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4  component
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 function\b0  EscapeRegExChars(\b const\b0  S: \b string\b0 ): \b string\b0 ;
\par \f0 
\par \b\fs22 Description
\par \b0\fs20 Escapes regex characters in S so that the regex engine can be used to match S as plain text\f2 .\f0 
\par }
1116
Scribble1116
EscapeRegExChars method - See also




Done



FALSE
6
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike RegEx property\cf3\strike0\{linkID=1050\}\cf0 
\par }
1117
Scribble1117
EscapeRegExChars method - Example




Done



FALSE
12
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil\fcharset0 Arial;}{\f2\fnil Courier New;}}
{\colortbl ;\red0\green0\blue255;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 EscapeRegExChars method example
\par \cf0\b0 
\par \f1 To search for '1+1' as literal text, use:
\par 
\par \f2 PerlRegEx1.RegEx := PerlRegEx1.EscapeRegExChars('1+1');\f1 
\par 
\par which does the same as:
\par 
\par \f2 PerlRegEx1.RegEx := '1\\+1';\f0 
\par }
1120
Scribble1120
Match method
Match,TPerlRegEx;TPerlRegEx,Match


perlregex:000230
Done


TPerlRegEx_Match;Match_Method;Match
FALSE
15
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fswiss Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 Match method
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1121\}\tab\cf2\strike Example\cf3\strike0\{linkID=%1122\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4  component
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 function\b0  Match: Boolean;
\par \f0 
\par \b\fs22 Description
\par \pard\b0\f2\fs20 Attempts to match the regular expression specified in the \cf2\strike RegEx property\cf3\strike0\{linkID=1050\}\cf0  on the string specified in the \cf2\strike Subject property\cf3\strike0\{linkID=1100\}\cf0 .  If \cf2\strike Compile\cf3\strike0\{linkID=1105\}\cf0  has not yet been called, Match will do so for you.
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640 Call \cf2\strike MatchAgain\cf3\strike0\{linkID=1125\}\cf0  to attempt to match the regex on the remainder of the subject string after a successful call to Match.\f0 
\par }
1121
Scribble1121
Match method - See also




Done



FALSE
14
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike RegEx property\cf3\strike0\{linkID=1050\}
\par \cf2\strike Options property\cf3\strike0\{linkID=1045\}
\par \cf2\strike Subject property\cf3\strike0\{linkID=1100\}
\par 
\par \cf2\strike Compile method\cf3\strike0\{linkID=1105\}
\par \cf2\strike Match method\cf3\strike0\{linkID=1120\}
\par \cf2\strike MatchAgain method\cf3\strike0\{linkID=1125\}
\par 
\par \cf2\strike OnMatch event\cf3\strike0\{linkID=1175\}\cf0 
\par }
1122
Scribble1122
Match method - Example




Done



FALSE
11
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fmodern Courier New;}}
{\colortbl ;\red0\green0\blue255;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 Match method example
\par \cf0\b0 
\par \b\f1 with\b0  PerlRegEx1 \b do\b0  \b begin\b0 
\par   RegEx := Edit1.Text;
\par   Subject := Edit2.Text;
\par   \b if\b0  Match \b then\b0  ShowMessage('The regex you entered in Edit1 matches the string in Edit2')
\par     \b else\b0  ShowMessage('The regex you entered in Edit1 does not match the string in Edit2');
\par \b end\b0 ;\f0 
\par }
1125
Scribble1125
MatchAgain method
MatchAgain,TPerlRegEx;TPerlRegEx,MatchAgain


perlregex:000240
Done


TPerlRegEx_MatchAgain;MatchAgain_Method;MatchAgain
FALSE
14
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 MatchAgain method
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1126\}\tab\cf2\strike Example\cf3\strike0\{linkID=%1127\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4  component
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 function\b0  MatchAgain: Boolean;
\par \f0 
\par \b\fs22 Description
\par \b0\fs20 Attempt to match the regex to the remainder of the string after the previous match\f2 .  If you assigned the \cf2\strike Start property\cf3\strike0\{linkID=1060\}\cf0 , MatchAgain continues from that position instead.  If not, you should only call MatchAgain after calling \cf2\strike Match\cf3\strike0\{linkID=1120\}\cf0 .\f0 
\par }
1126
Scribble1126
MatchAgain method - See also




Done



FALSE
9
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike Match method\cf3\strike0\{linkID=1120\}\cf0 
\par \cf2\strike Start property\cf3\strike0\{linkID=1060\}\cf0 
\par \cf2\strike Stop property\cf3\strike0\{linkID=1070\}
\par \cf2\strike OnMatch event\cf3\strike0\{linkID=1175\}\cf0 
\par }
1127
Scribble1127
MatchAgain method - Example




Done



FALSE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fmodern Courier New;}}
{\colortbl ;\red0\green0\blue255;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 MatchAgain method example
\par \cf0\b0 
\par \b\f1 with\b0  PerlRegEx1 \b do\b0  \b begin\b0 
\par   RegEx := Edit1.Text;
\par   Subject := Edit2.Text;
\par   \b if\b0  Match \b then\b0  \b begin\b0 
\par     I := 1;
\par     \b while\b0  MatchAgain \b do\b0  I := I + 1;
\par     ShowMessage(Format('The regular expression matched the subject string %d times.', [I]);
\par   \b end\b0 
\par   \b else\b0 
\par     ShowMessage('The regular expression did not match the subject string at all.');
\par \b end\b0 ;\f0 
\par }
1130
Scribble1130
NamedSubExpression method
NamedSubExpression,TPerlRegEx;TPerlRegEx,NamedSubExpression


perlregex:000250
Done


TPerlRegEx_NamedSubExpression;NamedSubExpression_Method;NamedSubExpression
FALSE
14
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 NamedSubExpression method
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1131\}\tab\cf2\strike Example\cf3\strike0\{linkID=%1132\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4  component
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 function\b0  NamedSubExpression(\b const\b0  SEName: \b string\b0 ): Integer;
\par \f0 
\par \b\fs22 Description
\par \b0\fs20 Returns the index of the named group SEName\f2 .  Use the returned value with the \cf2\strike SubExpressions property\cf3\strike0\{linkID=1095\}\cf0  to retrieve the text matched by the named group.\f0 
\par }
1131
Scribble1131
NamedSubExpression method - See also




Done



FALSE
9
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike SubExpressionCount property\cf3\strike0\{linkID=1080\}\cf0 
\par \cf2\strike SubExpressionLengths property\cf3\strike0\{linkID=1085\}\cf0 
\par \cf2\strike SubExpressionOffsets property\cf3\strike0\{linkID=1090\}\cf0 
\par \cf2\strike SubExpressions property\cf3\strike0\{linkID=1095\}\cf0 
\par }
1132
Scribble1132
NamedSubExpression method - Example




Done



FALSE
11
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fmodern Courier New;}{\f2\fmodern\fcharset0 Courier New;}}
{\colortbl ;\red0\green0\blue255;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 NamedSubExpression method example
\par \cf0\b0 
\par \b\f1 with\b0  PerlRegEx1 \b do\b0  \b begin\b0 
\par   RegEx := '(\f2 ?P<firstname>\f1 Joe|Jack|William|Avarell) did it';
\par   Subject := 'The sheriff told us that Joe did it.';
\par   \b if\b0  Match \b then\b0 
\par     ShowMessage('Wanted: ' + SubExpressions[\f2 NamedSubExpression('firstname')\f1 ]);
\par \b end\b0 ;\f0 
\par }
1135
Scribble1135
Replace method
Replace,TPerlRegEx;TPerlRegEx,Replace


perlregex:000260
Done


TPerlRegEx_Replace;Replace_Method;Replace
FALSE
15
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 Replace method
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1136\}\tab\cf2\strike Example\cf3\strike0\{linkID=%1137\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4  component
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 function\b0  Replace: \b string\b0 ;
\par \f0 
\par \b\fs22 Description
\par \b0\fs20 Replace matched expression in \cf2\strike Subject\cf3\strike0\{linkID=1100\}\cf0  with \cf2\strike ComputeReplacement\cf3\strike0\{linkID=1110\}\cf0 . Returns the actual replacement text from ComputeReplacement\f2 .  You may only call Replace after a successful call to \cf2\strike Match\cf3\strike0\{linkID=1120\}\cf0  or \cf2\strike MatchAgain\cf3\strike0\{linkID=1125\}\cf0 .\f0 
\par 
\par }
1136
Scribble1136
Replace method - See also




Done



FALSE
12
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike Replacement property\cf3\strike0\{linkID=1055\}
\par \cf2\strike Subject property\cf3\strike0\{linkID=1100\}
\par \cf2\strike ComputeReplacement method\cf3\strike0\{linkID=1110\}
\par \cf2\strike Match method\cf3\strike0\{linkID=1120\}
\par \cf2\strike MatchAgain method\cf3\strike0\{linkID=1125\}
\par \cf2\strike ReplaceAll method\cf3\strike0\{linkID=1140\}
\par \cf2\strike OnReplace event\cf3\strike0\{linkID=1180\}\cf0 
\par }
1137
Scribble1137
Replace method - Example




Done



FALSE
12
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fmodern Courier New;}}
{\colortbl ;\red0\green0\blue255;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 Replace method example
\par \cf0\b0 
\par \b\f1 with\b0  PerlRegEx1 \b do\b0  \b begin\b0 
\par   Subject := 'Bill loves Hillary';
\par   RegEx := 'Hillary';
\par   Replacement := 'Monica';
\par   \b if\b0  Match \b then\b0  Replace;
\par   ShowMessage(Subject);   \i // "Bill loves Monica"\i0 
\par \b end\b0 ;\f0 
\par }
1140
Scribble1140
ReplaceAll method
ReplaceAll,TPerlRegEx;TPerlRegEx,ReplaceAll


perlregex:000270
Done


TPerlRegEx_ReplaceAll;ReplaceAll_Method;ReplaceAll
FALSE
15
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 ReplaceAll method
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1141\}\tab\cf2\strike Example\cf3\strike0\{linkID=%1142\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4  component
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 function\b0  ReplaceAll: Boolean;
\par \f0 
\par \b\fs22 Description
\par \b0\f2\fs20 Replaces all matches of \cf2\strike RegEx\cf3\strike0\{linkID=1050\}\cf0  in \cf2\strike Subject\cf3\strike0\{linkID=1100\}\cf0  with \cf2\strike ComputeReplacement\cf3\strike0\{linkID=1110\}\cf0\f0 . Returns True if anything was replaced at all.
\par 
\par }
1141
Scribble1141
ReplaceAll method - See also




Done



FALSE
12
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike RegEx property\cf3\strike0\{linkID=1050\}\cf0 
\par \cf2\strike Replacement property\cf3\strike0\{linkID=1055\}\cf0 
\par \cf2\strike Subject property\cf3\strike0\{linkID=1100\}\cf0 
\par \cf2\strike Match method\cf3\strike0\{linkID=1120\}\cf0 
\par \cf2\strike MatchAgain method\cf3\strike0\{linkID=1125\}\cf0 
\par \cf2\strike Replace method\cf3\strike0\{linkID=1135\}
\par \cf2\strike OnReplace event\cf3\strike0\{linkID=1180\}\cf0 
\par }
1142
Scribble1142
ReplaceAll method - Example




Done



FALSE
11
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fswiss Arial;}{\f2\fmodern Courier New;}}
{\colortbl ;\red0\green0\blue255;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 ReplaceAll method example
\par \cf0\b0 
\par \pard\tx1435\tx2875\tx4315\tx5755\tx7195\tx8635\f1 ReplaceAll is the equivalent of:
\par 
\par \pard\b\f2 if\b0  Match \b then\b0  \b begin\b0 
\par   Replace;
\par   \b while\b0  MatchAgain \b do\b0  Replace;
\par \b end\b0 ;\f0 
\par }
1145
Scribble1145
Split method
Split,TPerlRegEx;TPerlRegEx,Split


perlregex:000280
Done


TPerlRegEx_Split;Split_Method;Split
FALSE
14
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil\fcharset0 Arial;}{\f2\fnil Courier New;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 Split method
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%114\f1 6\f0\}\tab\cf2\strike Example\cf3\strike0\{linkID=%1146\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4  component
\par 
\par \cf0\b\fs22 Declaration
\par \pard\f2\fs20 procedure\b0  Split(Strings: TStrings; Limit: Integer);
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\f0 
\par \b\fs22 Description
\par \b0\f1\fs20 Splits \cf2\strike Subject\cf3\strike0\{linkID=1100\}\cf0  along regex matches.  The text between each regex match is appended to the list passed in the Strings parameter.  If Limit >= 1, at most Limit strings are appended.\f0 
\par }
1146
Scribble1146
Split method - See also




Done



FALSE
10
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike RegEx property\cf3\strike0\{linkID=1050\}\cf0 
\par \cf2\strike Subject property\cf3\strike0\{linkID=1100\}\cf0 
\par \cf2\strike Match method\cf3\strike0\{linkID=1120\}\cf0 
\par \cf2\strike MatchAgain method\cf3\strike0\{linkID=1125\}\cf0 
\par 
\par }
1147
Scribble1147
Split method - Example




Done



FALSE
12
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil\fcharset0 Arial;}{\f1\fnil Arial;}{\f2\fswiss\fcharset0 Arial;}{\f3\fswiss Arial;}{\f4\fnil Courier New;}}
{\colortbl ;\red0\green0\blue255;}
\viewkind4\uc1\pard\cf1\b\fs20 Split\f1  method example
\par \cf0\b0 
\par \pard\tx1435\tx2875\tx4315\tx5755\tx7195\tx8635\f2 This example appends each item in the comma-delimited list in Edit1 to Memo1:\f3 
\par 
\par \pard\b\f4 with\b0  PerlRegEx1 \b do\b0  \b begin\b0 
\par   RegEx := ',';
\par   Subject := Edit1.Text;
\par   Split(Memo1.Lines, 0);
\par \b end\b0 ;\f1 
\par }
1150
Scribble1150
StoreSubExpressions method
StoreSubExpressions,TPerlRegEx;TPerlRegEx,StoreSubExpressions


perlregex:000290
Done


TPerlRegEx_StoreSubExpressions;StoreSubExpressions_Method;StoreSubExpressions
FALSE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 StoreSubExpressions method
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1151\}\tab\cf2\strike Example\cf3\strike0\{linkID=%1152\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4  component
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 procedure\b0  StoreSubExpressions;
\par \f0 
\par \b\fs22 Description
\par \b0\fs20 Stores duplicates of \cf2\strike SubExpressions[]\cf3\strike0\{linkID=1095\}\cf0  so they and \cf2\strike ComputeReplacement\cf3\strike0\{linkID=1110\}\cf0  will still return the proper strings\f2  \f0 even if \cf2\strike Subject\cf3\strike0\{linkID=1100\}\cf0  is changed or cleared\f2 .
\par 
\par This can be useful if you plan to modify the string that you assigned to Subject.  If you do that without clearing Subject first, Delphi will automatically create a duplicate of the subject string, which wastes memory and CPU time if the subject is very long.\f0 
\par }
1151
Scribble1151
StoreSubExpressions method - See also




Done



FALSE
7
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike SubExpressions property\cf3\strike0\{linkID=1095\}\cf0 
\par \cf2\strike Subject property\cf3\strike0\{linkID=1100\}\cf0 
\par }
1152
Scribble1152
StoreSubExpressions method - Example




Done



FALSE
37
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil\fcharset0 Arial;}{\f2\fnil Courier New;}{\f3\fmodern Courier New;}}
{\colortbl ;\red0\green0\blue255;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 StoreSubExpressions method example
\par \cf0\b0 
\par \f1 The first example displays "Nobody wanted":\f0 
\par 
\par \f2 S := 'The sheriff told us that Joe did it.';
\par \b with\b0  PerlRegEx1 \b do\b0  \b begin\b0 
\par   RegEx := '(Joe|Jack|William|Avarell) did it';
\par   Subject := S;
\par   \b if\b0  Match \b then\b0  \b begin\b0 
\par     Subject := ''; \i // Save memory\i0 
\par     \b if\b0  RegEx.SubExpressionCount = 1 \b then\b0 
\par       S := 'Wanted: ' + RegEx.SubExpressions[1]
\par     \b else\b0 
\par       S := 'Nobody wanted';
\par     ShowMessage(S);
\par   \b end\b0 ;
\par \b end\b0 ;\f3 
\par 
\par \f1 The second example displays "Wanted: Joe".
\par 
\par \f2 S := 'The sheriff told us that Joe did it.';
\par \b with\b0  PerlRegEx1 \b do\b0  \b begin\b0 
\par   RegEx := '(Joe|Jack|William|Avarell) did it';
\par   Subject := S;
\par   \b if\b0  Match \b then\b0  \b begin\b0 
\par     RegEx.StoreSubExpressions;
\par     Subject := ''; \i // Save memory\i0 
\par     \b if\b0  RegEx.SubExpressionCount = 1 \b then\b0 
\par       S := 'Wanted: ' + RegEx.SubExpressions[1]
\par     \b else\b0 
\par       S := 'Nobody wanted';
\par     ShowMessage(S);
\par   \b end\b0 ;
\par \b end\b0 ;\f0 
\par }
1155
Scribble1155
Study method
Study,TPerlRegEx;TPerlRegEx,Study


perlregex:000300
Done


TPerlRegEx_Study;Study_Method;Study
FALSE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}{\f3\fswiss Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 Study method
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1156\}\tab\cf2\strike Example\cf3\strike0\{linkID=%1157\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4  component
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 procedure\b0  Study;
\par \f0 
\par \b\fs22 Description
\par \b0\fs20 Study the regex. Studying takes time, but will make the execution of the regex a lot faster.
\par Call study if you will be using the same regex many times\f2 .
\par \f3 Study will also call \cf2\strike Compile\cf3\strike0\{linkID=1105\}\cf0  if this had not yet been done.\f0 
\par }
1156
Scribble1156
Study method - See also




Done



FALSE
8
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike Compiled property\cf3\strike0\{linkID=1020\}\cf0 
\par \cf2\strike Studied property\cf3\strike0\{linkID=1075\}\cf0 
\par \cf2\strike Compile method\cf3\strike0\{linkID=1105\}\cf0 
\par }
1157
Scribble1157
Study method - Example




Done



FALSE
15
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fswiss Arial;}{\f2\fmodern Courier New;}}
{\colortbl ;\red0\green0\blue255;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 Study method example
\par \cf0\b0 
\par \pard\tx1435\tx2875\tx4315\tx5755\tx7195\tx8635\f1 Depending on what the user entered in Edit1 and Memo1, RegEx might end up being a pretty complicated regular expression that wil be applied to the memo text a great many times.  This makes it worthwhile to spend a little extra time studying the regular expression.
\par 
\par \pard\b\f2 with\b0  PerlRegEx1 \b do\b0  \b begin\b0 
\par   RegEx := Edit1.Text;
\par   Study;
\par   Subject := Memo1.Lines.Text;
\par   Replacement := Edit2.Text;
\par   ReplaceAll;
\par   Memo1.Lines.Text := Subject;
\par \b end\b0 ;\f0 
\par }
1175
Scribble1175
OnMatch event
OnMatch,TPerlRegEx;TPerlRegEx,OnMatch


perlregex:000310
Done


TPerlRegEx_OnMatch;OnMatch_Event;OnMatch
TRUE
15
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 OnMatch event
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1176\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4  component
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 property\b0  OnMatch: TNotifyEvent;
\par \f0 
\par \b\fs22 Description
\par \b0\fs20 Triggered by \cf2\strike Match\cf3\strike0\{linkID=1120\}\cf0  and \cf2\strike MatchAgain\cf3\strike0\{linkID=1125\}\cf0  after a successful match\f2 .\f0 
\par 
\par }
1176
Scribble1176
OnMatch event - See also




Done



FALSE
7
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike Match method\cf3\strike0\{linkID=1120\}\cf0 
\par \cf2\strike MatchAgain method\cf3\strike0\{linkID=1125\}\cf0 
\par }
1180
Scribble1180
OnReplace event
OnReplace,TPerlRegEx;TPerlRegEx,OnReplace


perlregex:000320
Done


TPerlRegEx_OnReplace;OnReplace_Event;OnReplace
FALSE
14
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 OnReplace event
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1181\}\tab\cf2\strike Example\cf3\strike0\{linkID=%1182\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegEx\cf3\strike0\{linkID=1010\}\cf4  component
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 property\b0  OnReplace: \cf2\strike TPerlRegExReplaceEvent\cf3\strike0\{linkID=1305\}\cf0 ;
\par \f0 
\par \b\fs22 Description
\par \b0\fs20 Triggered by \cf2\strike Replace\cf3\strike0\{linkID=1135\}\cf0  and \cf2\strike ReplaceAll\cf3\strike0\{linkID=1140\}\cf0  just before the replacement is done, allowing you to determine the new string\f2 .\f0 
\par }
1181
Scribble1181
OnReplace event - See also




Done



FALSE
9
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike Replacement property\cf3\strike0\{linkID=1055\}
\par \cf2\strike ComputeReplacement method\cf3\strike0\{linkID=1110\}
\par \cf2\strike Replace method\cf3\strike0\{linkID=1135\}\f1 
\par \cf2\strike ReplaceAll method\cf3\strike0\{linkID=1140\}\cf0\f0 
\par }
1182
Scribble1182
OnReplace event - Example




Done



FALSE
18
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fmodern Courier New;}{\f2\fmodern\fcharset0 Courier New;}{\f3\fnil Courier New;}}
{\colortbl ;\red0\green0\blue255;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 OnReplace event example
\par \cf0\b0 
\par \b\f1 with\b0  PerlRegEx1 \b do\b0  \b begin\b0 
\par   RegEx := 'Borland\f2 |Inprise\f1 |Microsoft';
\par   Subject := Edit1.Text;
\par   ReplaceAll;
\par   Edit1.Text := Subject;
\par \b end\b0 ;
\par 
\par \b\f3 procedure\b0  TForm1.PerlRegEx1Replace(Sender: TObject; \b var\b0  ReplaceWith: \b string\b0 );
\par \b begin\b0 
\par   \b if\b0  PerlRegEx1.MatchedExpression = 'Inprise' \b then\b0  ReplaceWith := 'Borland'
\par     \b else\b0  \b if\b0  PerlRegEx1.MatchedExpression = 'Borland' \b then\b0  ReplaceWith := ':-)'
\par     \b else\b0  ReplaceWith := ':-('
\par \b end\b0 ;\f0 
\par }
1200
Scribble1200
TPerlRegExList class
TPerlRegExList


perlregex:000330
Done


TPerlRegExList_Object;TPerlRegExList
TRUE
13
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 TPerlRegExList class
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1201\}\tab\cf2\ul Properties\cf3\ulnone\{linkID=%1202\}\tab\cf2\ul Methods\cf3\ulnone\{linkID=%1203\}\cf4\{keepn\}\cf0\b\fs22 
\par Unit
\par \cf2\b0\strike\fs20 PerlRegEx\cf3\strike0\{linkID=1000\}\cf4 
\par 
\par \cf0\b\fs22 Description
\par \b0\f1\fs20 Y\f0 ou can add \cf2\strike TPerlRegEx components\cf3\strike0\{linkID=1010\}\cf0  to a TPerlRegExList to match them all together on the same subject,\f1  \f0 as if they were one regex regex1|regex2|regex3|...
\par 
\par TPerlRegExList does not own the TPerlRegEx components, just like a TList\f1 .  I\f0 f a TPerlRegEx has been added to a TPerlRegExList, it should not be used in any other situation\f1  \f0 until it is removed from the list\f1 .\f0 
\par }
1201
Scribble1201
TPerlRegExList class - See also




Done



FALSE
6
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike TPerlRegEx component\cf3\strike0\{linkID=1010\}\cf0 
\par }
1202
Scribble1202
TPerlRegExList - Properties




Done



FALSE
13
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red128\green0\blue0;\red0\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 Properties
\par \cf0\b0 
\par \cf2\{bmct runtime.bmp\}\cf3  Run-time only\tab\cf2\{bmct key.bmp\}\cf3  Key properties
\par \pard\tx200\tx640\cf2\{bmct runtime.bmp\}\tab\{bmct key.bmp\}\tab\cf4\strike Count\cf2\strike0\{linkID=1210\}\cf3 
\par \cf2\{bmct runtime.bmp\}\tab\{bmct key.bmp\}\tab\cf4\strike MatchedRegEx\cf2\strike0\{linkID=1215\}\cf3 
\par \cf2\{bmct runtime.bmp\}\tab\{bmct key.bmp\}\tab\cf4\strike RegEx\cf2\strike0\{linkID=1220\}\cf3 
\par \cf2\{bmct runtime.bmp\}\tab\{bmct key.bmp\}\tab\cf4\strike Start\cf2\strike0\{linkID=1225\}\cf3 
\par \cf2\{bmct runtime.bmp\}\tab\{bmct key.bmp\}\tab\cf4\strike Stop\cf2\strike0\{linkID=1230\}\cf3 
\par \cf2\{bmct runtime.bmp\}\tab\{bmct key.bmp\}\tab\cf4\strike Subject\cf2\strike0\{linkID=1235\}\cf3 
\par \cf0 
\par }
1203
Scribble1203
TPerlRegExList - Methods




Done



FALSE
14
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red128\green0\blue0;\red0\green0\blue0;\red0\green128\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 Methods
\par \cf0\b0 
\par \cf2\{bmct key.bmp\}\cf3  Key methods
\par \pard\tx200\tx640\f1\tab\cf2\{bmct key.bmp\}\tab\cf4\strike Add\cf2\strike0\{linkID=1245\}\cf3 
\par \tab\cf2\{bmct key.bmp\}\tab\cf4\strike IndexOf\cf2\strike0\{linkID=1250\}\cf3 
\par \tab\cf2\{bmct key.bmp\}\tab\cf4\strike Match\cf2\strike0\{linkID=1255\}\cf3 
\par \tab\cf2\{bmct key.bmp\}\tab\cf4\strike MatchAgain\cf2\strike0\{linkID=1260\}\cf3 
\par \tab\cf2\{bmct key.bmp\}\tab\cf4\strike Clear\cf2\strike0\{linkID=1265\}\cf3 
\par \tab\cf2\{bmct key.bmp\}\tab\cf4\strike Delete\cf2\strike0\{linkID=1270\}\cf3 
\par \tab\cf2\{bmct key.bmp\}\tab\cf4\strike Insert\cf2\strike0\{linkID=1275\}\cf3 
\par \cf0\f0 
\par }
1210
Scribble1210
Count property
Count,TPerlRegExList;TPerlRegExList,Count


perlregex:000340
Done


TPerlRegExList_Count;Count_Property;Count
FALSE
15
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green0\blue0;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 Count property
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf3\b0\strike\fs20 TPerlRegExList\cf4\strike0\{linkID=1200\}\cf2  class
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 property\b0  Count: Integer;
\par \f0 
\par \b\fs22 Description
\par \b0\f2\fs20 Number of \cf3\strike TPerlRegEx\cf4\strike0\{linkID=1010\}\cf0  components in the list.\f0 
\par Run-time and read-only.
\par }
1215
Scribble1215
MatchedRegEx property
MatchedRegEx,TPerlRegExList;TPerlRegExList,MatchedRegEx


perlregex:000350
Done


TPerlRegExList_MatchedRegEx;MatchedRegEx_Property;MatchedRegEx
FALSE
15
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 MatchedRegEx property
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1216\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegExList\cf3\strike0\{linkID=1200\}\cf4  class
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 property\b0  MatchedRegEx: TPerlRegEx;
\par \f0 
\par \b\fs22 Description
\par \b0\f2\fs20 Indicates which \cf2\strike TPerlRegEx\cf3\strike0\{linkID=1010\}\cf0  component in the list was responsible for finding the last match.\f0 
\par Run-time and read-only.
\par }
1216
Scribble1216
MatchedRegEx property - See also




Done



FALSE
7
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike Match method\cf3\strike0\{linkID=1255\}\cf0 
\par \cf2\strike MatchAgain method\cf3\strike0\{linkID=1260\}\cf0 
\par }
1220
Scribble1220
RegEx property
RegEx,TPerlRegExList;TPerlRegExList,RegEx


perlregex:000360
Done


TPerlRegExList_RegEx;RegEx_Property;RegEx
FALSE
15
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Courier New;}{\f3\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 RegEx property
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1221\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegExList\cf3\strike0\{linkID=1200\}\cf4  class
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 property\b0  RegEx[Index: Integer\f2 ]\f1 : TPerlRegEx;
\par \f0 
\par \b\fs22 Description
\par \b0\f3\fs20 The regular expression of each \cf2\strike TPerlRegEx component\cf3\strike0\{linkID=1010\}\cf0  in the list.\f0 
\par Run-time only\f3 .\f0 
\par }
1221
Scribble1221
RegEx property - See also




Done



FALSE
7
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike Count property\cf3\strike0\{linkID=1210\}\cf2\strike 
\par Add method\cf3\strike0\{linkID=1245\}\cf0 
\par }
1225
Scribble1225
Start property
Start,TPerlRegExList;TPerlRegExList,Start


perlregex:000370
Done


TPerlRegExList_Start;Start_Property;Start
FALSE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 Start property
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1226\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegExList\cf3\strike0\{linkID=1200\}\cf4  class
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 property\b0  Start: Integer;
\par \f0 
\par \b\fs22 Description
\par \b0\fs20 Starting position in \cf2\strike Subject\cf3\strike0\{linkID=\f2 1235\f0\}\cf0  from which \cf2\strike MatchAgain\cf3\strike0\{linkID=\f2 1260\f0\}\cf0  begins\f2 .
\par 
\par By default, MatchAgain continues from the end of the previous match, or from the start of the subject if there is no previous match.  Set the Start property to continue searching from another position.\f0 
\par }
1226
Scribble1226
Start property - See also




Done



FALSE
8
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike Stop property\cf3\strike0\{linkID=1230\}\cf0 
\par \cf2\strike Subject property\cf3\strike0\{linkID=1235\}\cf0 
\par \cf2\strike MatchAgain method\cf3\strike0\{linkID=1260\}\cf0 
\par }
1230
Scribble1230
Stop property
Stop,TPerlRegExList;TPerlRegExList,Stop


perlregex:000380
Done


TPerlRegExList_Stop;Stop_Property;Stop
FALSE
14
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil\fcharset0 Arial;}{\f2\fnil Courier New;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 Stop property
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1231\f1\}\cf4\f0\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegExList\cf3\strike0\{linkID=1200\}\cf4  class
\par 
\par \cf0\b\fs22 Declaration
\par \f2\fs20 property\b0  Stop: Integer;
\par \f0 
\par \b\fs22 Description
\par \b0\fs20 Last character in \cf2\strike Subject\cf3\strike0\{linkID=1235\}\cf0  that \cf2\strike Match\cf3\strike0\{linkID=\f1 1255\f0\}\cf0  and \cf2\strike MatchAgain\cf3\strike0\{linkID=1\f1 260\f0\}\cf0  search through\f1 .  By default, they search until the end of the string.  Use the Stop property to search through only part of the string without having to reallocate a truncated string.\f0 
\par }
1231
Scribble1231
Stop property - See also




Done



FALSE
9
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike Start property\cf3\strike0\{linkID=1225\}\cf0 
\par \cf2\strike Subject property\cf3\strike0\{linkID=1235\}\cf0 
\par \cf2\strike Match method\cf3\strike0\{linkID=1255\}\cf0 
\par \cf2\strike MatchAgain method\cf3\strike0\{linkID=1260\}\cf0 
\par }
1235
Scribble1235
Subject property
Subject,TPerlRegExList;TPerlRegExList,Subject


perlregex:000390
Done


TPerlRegExList_Subject;Subject_Property;Subject
FALSE
15
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 Subject property
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1236\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegExList\cf3\strike0\{linkID=1200\}\cf4  class
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 property\b0  Subject: \b string\b0 ;
\par \f0 
\par \b\fs22 Description
\par \b0\fs20 The string on which \cf2\strike Match\cf3\strike0\{linkID=1\f2 255\f0\}\cf0  will try to match\f2  the regular expressions.\f0 
\par Run-time only\f2 .\f0 
\par }
1236
Scribble1236
Subject property - See also




Done



FALSE
8
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike Add method\cf3\strike0\{linkID=1245\}\cf2\strike 
\par Match method\cf3\strike0\{linkID=1255\}\cf0 
\par \cf2\strike MatchAgain method\cf3\strike0\{linkID=1260\}\cf0 
\par }
1245
Scribble1245
Add method
Add,TPerlRegExList;TPerlRegExList,Add


perlregex:000400
Done


TPerlRegExList_Add;Add_Method;Add
FALSE
14
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 Add method
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1246\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegExList\cf3\strike0\{linkID=1200\}\cf4  class
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 function\b0  Add(ARegEx: TPerlRegEx): Integer;
\par \f0 
\par \b\fs22 Description
\par \b0\f2\fs20 Add a regular expression to the list.\f0 
\par }
1246
Scribble1246
Add method - See also




Done



FALSE
7
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike Delete method\cf3\strike0\{linkID=1270\}\cf0 
\par \cf2\strike Insert method\cf3\strike0\{linkID=1275\}\cf0 
\par }
1250
Scribble1250
IndexOf method
IndexOf,TPerlRegExList;TPerlRegExList,IndexOf


perlregex:000410
Done


TPerlRegExList_IndexOf;IndexOf_Method;IndexOf
FALSE
14
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 IndexOf method
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1251\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegExList\cf3\strike0\{linkID=1200\}\cf4  class
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 function\b0  IndexOf(ARegEx: TPerlRegEx): Integer;
\par \f0 
\par \b\fs22 Description
\par \b0\f2\fs20 Returns the index of the given \cf2\strike TPerlRegEx component\cf3\strike0\{linkID=1010\}\cf0  in the list.  Returns -1 if the list does not contain the component.\f0 
\par }
1251
Scribble1251
IndexOf method - See also




Done



FALSE
8
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike Add method\cf3\strike0\{linkID=1245\}\cf0 
\par \cf2\strike Delete method\cf3\strike0\{linkID=1270\}\cf0 
\par \cf2\strike Insert method\cf3\strike0\{linkID=1275\}\cf0 
\par }
1255
Scribble1255
Match method
Match,TPerlRegExList;TPerlRegExList,Match


perlregex:000420
Done


TPerlRegExList_Match;Match_Method;Match
FALSE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fswiss Arial;}{\f3\fswiss\fcharset0 Arial;}{\f4\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 Match method
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1256\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegExList\cf3\strike0\{linkID=1200\}\cf4  class
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 function\b0  Match: Boolean;
\par \f0 
\par \b\fs22 Description
\par \pard\b0\f2\fs20 Attempts to match \f3 the regular expressions in the list \f2 on the string specified in the \cf2\strike Subject property\cf3\strike0\{linkID=1\f3 235\f2\}\cf0 .  Call \cf2\strike MatchAgain\cf3\strike0\{linkID=1\f3 260\f2\}\cf0  to attempt to match the regex on the remainder of the subject string after a successful call to Match.
\par 
\par \f3 If more than one regular expression matches the subject, the leftmost match (i.e. the one with the lowest \cf2\strike MatchedExpressionOffset\cf3\strike0\{linkID=1040\}\cf0\f4 ) \f3 is returned by the \cf2\strike MatchedRegEx property\cf3\strike0\{linkID=1215\}\cf0\f4 .  If more than one regular expression matches at the same position in the string, the one added to the list first is returned.  Use the MatchedRegEx property to get more information about the match.\f0 
\par }
1256
Scribble1256
Match method - See also




Done



FALSE
7
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike MatchedRegEx property\cf3\strike0\{linkID=1215\}\cf2\strike 
\par MatchAgain method\cf3\strike0\{linkID=1260\}\cf0 
\par }
1260
Scribble1260
MatchAgain method
MatchAgain,TPerlRegExList;TPerlRegExList,MatchAgain


perlregex:000430
Done


TPerlRegExList_MatchAgain;MatchAgain_Method;MatchAgain
FALSE
18
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}{\f3\fswiss\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 MatchAgain method
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1261\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegExList\cf3\strike0\{linkID=1200\}\cf4  class
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 function\b0  MatchAgain: Boolean;
\par \f0 
\par \b\fs22 Description
\par \b0\fs20 Attempt to match the regex to the remainder of the string after the previous match\f2 .  If you assigned the \cf2\strike Start property\cf3\strike0\{linkID=1060\}\cf0 , MatchAgain continues from that position instead.  If not, you should only call MatchAgain after calling \cf2\strike Match\cf3\strike0\{linkID=1120\}\cf0 .
\par 
\par \f3 If more than one regular expression matches the subject, the leftmost match (i.e. the one with the lowest \cf2\strike MatchedExpressionOffset\cf3\strike0\{linkID=1040\}\cf0\f2 ) \f3 is returned by the \cf2\strike MatchedRegEx property\cf3\strike0\{linkID=1215\}\cf0\f2 .  If more than one regular expression matches at the same position in the string, the one added to the list first is returned.  Use the MatchedRegEx property to get more information about the match.
\par 
\par Note that MatchAgain continues from the end of the previous match.  Any matches by other regular expressions in the list that overlap with the previous match are ignored.  This is the same behavior as you would get when stringing all the regular expressions together as regex1|regex2|regex3... and using \cf2\strike TPerlRegEx.MatchAgain\cf3\strike0\{linkID=1125\}\cf0 .\f0 
\par }
1261
Scribble1261
MatchAgain method - See also




Done



FALSE
9
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike MatchedRegEx property\cf3\strike0\{linkID=1215\}\cf0 
\par \cf2\strike Start property\cf3\strike0\{linkID=1225\}\cf0 
\par \cf2\strike Stop property\cf3\strike0\{linkID=1230\}\cf0 
\par \cf2\strike Match method\cf3\strike0\{linkID=1255\}\cf0 
\par }
1265
Scribble1265
Clear method
Clear,TPerlRegExList;TPerlRegExList,Clear


perlregex:000440
Done


TPerlRegExList_Clear;Clear_Method;Clear
FALSE
15
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 Clear method
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1266\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegExList\cf3\strike0\{linkID=1200\}\cf4  class
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 procedure\b0  Clear;
\par \f0 
\par \b\fs22 Description
\par \b0\f2\fs20 Removes all regular expressions from the list.  The TPerlRegEx components are NOT freed.\f0 
\par 
\par }
1266
Scribble1266
Clear method - See also




Done



FALSE
6
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike Delete method\cf3\strike0\{linkID=1270\}\cf0 
\par }
1270
Scribble1270
Delete method
Delete,TPerlRegExList;TPerlRegExList,Delete


perlregex:000450
Done


TPerlRegExList_Delete;Delete_Method;Delete
FALSE
15
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 Delete method
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1271\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegExList\cf3\strike0\{linkID=1200\}\cf4  class
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 procedure\b0  Delete(Index: Integer);
\par \f0 
\par \b\fs22 Description
\par \b0\f2\fs20 Remove a \cf2\strike TPerlRegEx component\cf3\strike0\{linkID=1010\}\cf0  from the list.  The component is NOT freed.\f0 
\par 
\par }
1271
Scribble1271
Delete method - See also




Done



FALSE
7
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike Add method\cf3\strike0\{linkID=1245\}\cf0 
\par \cf2\strike Clear method\cf3\strike0\{linkID=1265\}\cf0 
\par }
1275
Scribble1275
Insert method
Insert,TPerlRegExList;TPerlRegExList,Insert


perlregex:000460
Done


TPerlRegExList_Insert;Insert_Method;Insert
FALSE
14
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 Insert method
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1276\}\cf4\{keepn\}\cf0\b\fs22 
\par Applies to
\par \cf2\b0\strike\fs20 TPerlRegExList\cf3\strike0\{linkID=1200\}\cf4  class
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 procedure\b0  Insert(Index: Integer; ARegEx: TPerlRegEx);
\par \f0 
\par \b\fs22 Description
\par \b0\f2\fs20 Add a regular expression to the list at a particular position.  The order of the regular expressions in the list matches only if two regular expressions match at the same position in the subject string.\f0 
\par }
1276
Scribble1276
Insert method - See also




Done



FALSE
7
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike Add method\cf3\strike0\{linkID=1245\}\cf0 
\par \cf2\strike Match method\cf3\strike0\{linkID=1255\}\cf0 
\par }
1300
Scribble1300
TPerlRegExOptions type
TPerlRegExOptions


perlregex:000470
Done



TRUE
22
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 TPerlRegExOptions type
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1301\}\cf4\{keepn\}\cf0\b\fs22 
\par Unit
\par \cf2\b0\strike\fs20 PerlRegEx\cf3\strike0\{linkID=1000\}\cf4 
\par 
\par \cf0\b\fs22 Declaration
\par \pard\f1\fs20 type\b0 
\par   TPerlRegExOptions = \b set\b0  \b of\b0  (
\par     preCaseLess,       \i // /i -> Case insensitive\i0 
\par     preMultiLine,      \i // /m -> ^ and $ also match before/after a newline, not just at the beginning and the end of the string\i0 
\par     preSingleLine,     \i // /s -> Dot matches any character, including \\n (newline). Otherwise, it matches anything except \\n\i0 
\par     preExtended,       \i // /x -> Allow regex to contain extra whitespace, newlines and Perl-style comments, all of which will be filtered out\i0 
\par     preAnchored,       \i // /A -> Successful match can only occur at the start of the subject or right after the previous match\i0 
\par     preDollarEndOnly,  \i // /E\i0 
\par     preExtra,          \i // /X\i0 
\par     preUnGreedy        \i // Repeat operators (+, *, ?) are not greedy by default\i0 
\par   );                   \i //   (i.e. they try to match the minimum number of characters instead of the maximum)\i0 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\f0 
\par }
1301
Scribble1301
TPerlRegExOptions type - See also




Done



FALSE
7
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike TPerlRegEx component\cf3\strike0\{linkID=1010\}
\par \cf2\strike Options property\cf3\strike0\{linkID=1045\}\cf0 
\par }
1305
Scribble1305
TPerlRegExReplaceEvent type
TPerlRegExReplaceEvent


perlregex:000480
Done



TRUE
17
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}{\f3\fswiss Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 TPerlRegExReplaceEvent type
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1306\}\cf4\{keepn\}\cf0\b\fs22 
\par Unit
\par \cf2\b0\strike\fs20 PerlRegEx\cf3\strike0\{linkID=1000\}\cf4 
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 type\b0  TPerlRegExReplaceEvent = \b procedure\b0 (Sender: TObject; \b var\b0  ReplaceWith: \b string\b0 ); \b of\b0  \b object\b0 ;
\par \f0 
\par \b\fs22 Description
\par \b0\f2\fs20 TPerlRegEx.\cf2\strike OnReplace\cf3\strike0\{linkID=1180\}\cf0  event type.
\par 
\par Sender is the calling \cf2\strike TPerlRegEx component\cf3\strike0\{linkID=1010\}\cf0 .
\par ReplaceWith is equal to the string returned by \cf2\strike ComputeReplacement method\cf3\strike0\{linkID=1110\}\cf0 .  \f3 The matched expression will be replaced with whatever you assign to ReplaceWith.\f0 
\par }
1306
Scribble1306
TPerlRegExReplaceEvent type - See also




Done



FALSE
7
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike TPerlRegEx component\cf3\strike0\{linkID=1010\}\cf0 
\par \cf2\strike OnReplace event\cf3\strike0\{linkID=1180\}\cf0 
\par }
1310
Scribble1310
TPerlRegExState type
TPerlRegExState


perlregex:000490
Done



TRUE
16
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 TPerlRegExState type
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\cf2\ul See also\cf3\ulnone\{linkID=%1311\}\cf4\{keepn\}\cf0\b\fs22 
\par Unit
\par \cf2\b0\strike\fs20 PerlRegEx\cf3\strike0\{linkID=1000\}\cf4 
\par 
\par \cf0\b\fs22 Declaration
\par \pard\b0\f1\fs20   TPerlRegExState = \b set\b0  \b of\b0  (
\par     preNotBOL,         \i // Not Beginning Of Line: ^ does not match at the start of Subject\i0 
\par     preNotEOL,         \i // Not End Of Line: $ does not match at the end of Subject\i0 
\par     preNotEmpty        \i // Empty matches not allowed\i0 
\par   );
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\f0 
\par }
1311
Scribble1311
TPerlRegExState type - See also




Done



FALSE
7
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs20 See also
\par \cf0\b0 
\par \cf2\strike TPerlRegEx component\cf3\strike0\{linkID=1010\}\cf0 
\par \cf2\strike State property\cf3\strike0\{linkID=1065\}\cf0 
\par }
1500
Scribble1500
MAX_SUBEXPRESSIONS constant
MAX_SUBEXPRESSIONS


perlregex:000500
Done



TRUE
14
{\rtf1\ansi\ansicpg1252\deff0\deflang1033{\fonttbl{\f0\fnil Arial;}{\f1\fnil Courier New;}{\f2\fnil\fcharset0 Arial;}}
{\colortbl ;\red0\green0\blue255;\red0\green128\blue0;\red128\green0\blue0;\red0\green0\blue0;}
\viewkind4\uc1\pard\cf1\b\f0\fs32 MAX_SUBEXPRESSIONS constant
\par \cf0\b0\fs16 
\par \pard\tx1440\tx2880\tx4320\tx5760\tx7200\tx8640\{keepn\}\b\fs22 
\par Unit
\par \cf2\b0\strike\fs20 PerlRegEx\cf3\strike0\{linkID=1000\}\cf4 
\par 
\par \cf0\b\fs22 Declaration
\par \f1\fs20 const\b0  MAX_SUBEXPRESSIONS = 99
\par \f0 
\par \b\fs22 Description
\par \b0\f2\fs20 Maximum number of capturing groups you can use in your regular expression.\f0 
\par }
0
0
0
52
1 TPerlRegEx component
2 TPerlRegEx component Reference=Scribble1010
2 Properties
3 Compiled=Scribble1020
3 FoundMatch=Scribble1025
3 MatchedExpression=Scribble1030
3 MatchedExpressionLength=Scribble1035
3 MatchedExpressionOffset=Scribble1040
3 Options=Scribble1045
3 RegEx=Scribble1050
3 Replacement=Scribble1055
3 Start=Scribble1060
3 State=Scribble1065
3 Stop=Scribble1070
3 Studied=Scribble1075
3 SubExpressionCount=Scribble1080
3 SubExpressionLengths=Scribble1085
3 SubExpressionOffsets=Scribble1090
3 SubExpressions=Scribble1095
3 Subject=Scribble1100
2 Methods
3 Compile=Scribble1105
3 ComputeReplacement=Scribble1110
3 EscapeRegExChars=Scribble1115
3 Match=Scribble1120
3 MatchAgain=Scribble1125
3 NamedSubExpression=Scribble1130
3 Replace=Scribble1135
3 ReplaceAll=Scribble1140
3 StoreSubExpressions=Scribble1150
3 Study=Scribble1155
2 Events
3 OnMatch=Scribble1175
3 OnReplace=Scribble1180
1 TPerlRegExList class
2 TPerlRegExList class Reference=Scribble1200
2 Properties
3 Count=Scribble1210
3 MatchedRegEx=Scribble1215
3 RegEx=Scribble1220
3 Start=Scribble1225
3 Stop=Scribble1230
3 Subject=Scribble1235
2 Methods
3 Create=Scribble1240
3 Add=Scribble1245
3 IndexOf=Scribble1250
3 Match=Scribble1255
3 MatchAgain=Scribble1260
3 Clear=Scribble1265
3 Delete=Scribble1270
3 Insert=Scribble1275
7
*InternetLink
16711680
Courier New
0
10
1
....
0
0
0
0
0
0
*ParagraphTitle
-16777208
Arial
0
11
1
B...
0
0
0
0
0
0
*PopupLink
-16777208
Arial
0
8
1
....
0
0
0
0
0
0
*PopupTopicTitle
16711680
Arial
0
10
1
B...
0
0
0
0
0
0
*SourceCode
-16777208
Courier New
0
10
1
....
0
0
0
0
0
0
*TopicText
-16777208
Arial
0
10
1
....
0
0
0
0
0
0
*TopicTitle
16711680
Arial
0
16
1
B...
0
0
0
0
0
0
