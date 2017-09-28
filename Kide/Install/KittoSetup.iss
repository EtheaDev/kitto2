#define MyAppName "Kitto2 and Kide2 IDE"

#define KittoVersion "2.0"
#define MyAppVersion "2.0.1"
#define MyAppExeName "Kide2.exe"
#define MyCopyRight "Copyright (c) 2012-2017 Ethea S.r.l."
#define MyCompany "Ethea S.r.l."

#define KITTO "..\.."

[Setup]
AppId={{052FA381-75D0-4B82-B6AD-FB9EF1967663}
DefaultDirName={sd}\Dev\Kitto_{#KittoVersion}
OutputBaseFilename=Kitto2Setup

AppName={#MyAppName}
AppVersion={#KittoVersion}
AppVerName={#MyAppName} {#MyAppVersion}
WizardImageFile=WizEtheaImage.bmp
WizardSmallImageFile=WizEtheaSmallImage.bmp
DefaultGroupName={#MyAppName}
DisableProgramGroupPage=true
Compression=lzma
SolidCompression=true
;ShowLanguageDialog=yes
AppCopyright={#MyCopyRight}
AppPublisher={#MyCompany}
SetupIconFile=.\kitto_64.ico
VersionInfoCompany={#MyCompany}
VersionInfoDescription={#MyAppName}
VersionInfoCopyright={#MyCopyRight}
VersionInfoProductName={#MyAppName}
VersionInfoProductVersion={#KittoVersion}
VersionInfoProductTextVersion={#KittoVersion}
VersionInfoVersion={#MyAppVersion}
ChangesAssociations=Yes
DisableWelcomePage=False
ShowLanguageDialog=auto

[Languages]
Name: eng; MessagesFile: compiler:Default.isl; LicenseFile: .\License_ENG.rtf
Name: ita; MessagesFile: compiler:Languages\Italian.isl; LicenseFile: .\License_ITA.rtf


[Tasks]
Name: desktopicon; Description: {cm:CreateDesktopIcon}; GroupDescription: {cm:AdditionalIcons}; Flags: unchecked
Name: quicklaunchicon; Description: {cm:CreateQuickLaunchIcon}; GroupDescription: {cm:AdditionalIcons}; Flags: unchecked; OnlyBelowVersion: 0,6.1

[Files]
;KIDE SECTION
Source: {#KITTO}\Kide\Bin\Kide2.exe; DestDir: {app}\kitto\Kide\Bin; Flags: ignoreversion; Components: Kide2
Source: {#KITTO}\Kide\Bin\Config.yaml; DestDir: {app}\kitto\Kide\Bin; Flags: ignoreversion; Components: Kide2
Source: {#KITTO}\Kide\Bin\*.png; DestDir: {app}\kitto\Kide\Bin; Flags: ignoreversion; Components: Kide2
Source: {#KITTO}\Kide\Bin\*.wav; DestDir: {app}\kitto\Kide\Bin; Flags: ignoreversion; Components: Kide2
Source: {#KITTO}\Kide\Bin\*.jpg; DestDir: {app}\kitto\Kide\Bin; Flags: ignoreversion; Components: Kide2
Source: {#KITTO}\Kide\Bin\ProjectTemplates\*; DestDir: {app}\kitto\Kide\Bin\ProjectTemplates; Flags: ignoreversion recursesubdirs createallsubdirs; Components: Kide2
Source: {#KITTO}\Kide\Bin\MetadataTemplates\*; DestDir: {app}\kitto\Kide\Bin\MetadataTemplates; Flags: ignoreversion recursesubdirs createallsubdirs; Components: Kide2
Source: {#KITTO}\Kide\Docs\*; DestDir: {app}\kitto\Kide\Docs; Flags: ignoreversion recursesubdirs createallsubdirs; Components: Kide2
Source: {#KITTO}\Kide\Ext\*; DestDir: {app}\kitto\Kide\Ext; Flags: ignoreversion recursesubdirs createallsubdirs; Components: Kide2
Source: {#KITTO}\Kide\Help\*; DestDir: {app}\kitto\Kide\Help; Flags: ignoreversion recursesubdirs createallsubdirs; Components: Kide2
Source: {#KITTO}\Kide\Projects\*.ico; DestDir: {app}\kitto\Kide\Projects; Flags: ignoreversion recursesubdirs createallsubdirs; Components: Kide2
Source: {#KITTO}\Kide\Projects\*.dpr; DestDir: {app}\kitto\Kide\Projects; Flags: ignoreversion recursesubdirs createallsubdirs; Components: Kide2
Source: {#KITTO}\Kide\Projects\*.dproj; DestDir: {app}\kitto\Kide\Projects; Flags: ignoreversion recursesubdirs createallsubdirs; Components: Kide2
Source: {#KITTO}\Kide\Projects\*.res; DestDir: {app}\kitto\Kide\Projects; Flags: ignoreversion recursesubdirs createallsubdirs; Components: Kide2
Source: {#KITTO}\Kide\Source\*.pas; DestDir: {app}\kitto\Kide\Source; Flags: ignoreversion recursesubdirs createallsubdirs; Components: Kide2
Source: {#KITTO}\Kide\Source\*.dfm; DestDir: {app}\kitto\Kide\Source; Flags: ignoreversion recursesubdirs createallsubdirs; Components: Kide2
Source: {#KITTO}\Kide\Source\*.inc; DestDir: {app}\kitto\Kide\Source; Flags: ignoreversion recursesubdirs createallsubdirs; Components: Kide2
Source: {#KITTO}\Kide\Source\*.rc; DestDir: {app}\kitto\Kide\Source; Flags: ignoreversion recursesubdirs createallsubdirs; Components: Kide2

;KITTO SECTION
Source: {#KITTO}\Source\*; DestDir: {app}\kitto\Source; Flags: ignoreversion recursesubdirs createallsubdirs; Components: Kitto2
;Source: {#KITTO}\Bin\*.yaml; DestDir: {app}\kitto\Bin; Flags: ignoreversion; Components: Kitto2
Source: {#KITTO}\Docs\External\*.txt; DestDir: {app}\kitto\Docs\External; Flags: ignoreversion; Components: Kitto2
Source: {#KITTO}\Docs\External\*.png; DestDir: {app}\kitto\Docs\External; Flags: ignoreversion; Components: Kitto2Docs
Source: {#KITTO}\Home\*; DestDir: {app}\kitto\Home; Flags: ignoreversion recursesubdirs createallsubdirs; Components: Kitto2
Source: .\MyProjectsFolder.txt; DestDir: {app}\kitto\MyProjects; Flags: ignoreversion; Components: Kitto2
;Source: {#KITTO}\Docs\External\Reference\Html\*; DestDir: {app}\kitto\Docs\Reference; Flags: ignoreversion recursesubdirs createallsubdirs; Components: Kitto2Docs

;KITTO EXAMPLES SECTION
Source: {#KITTO}\Examples\HelloKitto\DB\*.sql; DestDir: {app}\kitto\Examples\HelloKitto\DB; Flags: ignoreversion; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\DB\*.fbk; DestDir: {app}\kitto\Examples\HelloKitto\DB; Flags: ignoreversion; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Home\HelloKitto.exe; DestDir: {app}\kitto\Examples\HelloKitto\Home; Flags: ignoreversion; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Home\HelloKitto.kproj; DestDir: {app}\kitto\Examples\HelloKitto\Home; Flags: ignoreversion; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Home\Locale\*; DestDir: {app}\kitto\Examples\HelloKitto\Home\Locale; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Home\Metadata\*; DestDir: {app}\kitto\Examples\HelloKitto\Home\Metadata; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Home\Resources\*; DestDir: {app}\kitto\Examples\HelloKitto\Home\Resources; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Home\ReportTemplates\*; DestDir: {app}\kitto\Examples\HelloKitto\Home\ReportTemplates; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\*.ico; DestDir: {app}\kitto\Examples\HelloKitto\Projects; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\DXE7\*.dpr; DestDir: {app}\kitto\Examples\HelloKitto\Projects\DXE7; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\DXE7\*.dproj; DestDir: {app}\kitto\Examples\HelloKitto\Projects\DXE7; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\DXE7\*.res; DestDir: {app}\kitto\Examples\HelloKitto\Projects\DXE7; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\DXE8\*.dpr; DestDir: {app}\kitto\Examples\HelloKitto\Projects\DXE8; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\DXE8\*.dproj; DestDir: {app}\kitto\Examples\HelloKitto\Projects\DXE8; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\DXE8\*.res; DestDir: {app}\kitto\Examples\HelloKitto\Projects\DXE8; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\D10\*.dpr; DestDir: {app}\kitto\Examples\HelloKitto\Projects\D10; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\D10\*.dproj; DestDir: {app}\kitto\Examples\HelloKitto\Projects\D10; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\D10\*.res; DestDir: {app}\kitto\Examples\HelloKitto\Projects\D10; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\D10_1\*.dpr; DestDir: {app}\kitto\Examples\HelloKitto\Projects\D10_1; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\D10_1\*.dproj; DestDir: {app}\kitto\Examples\HelloKitto\Projects\D10_1; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\D10_1\*.res; DestDir: {app}\kitto\Examples\HelloKitto\Projects\D10_1; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\D10_2\*.dpr; DestDir: {app}\kitto\Examples\HelloKitto\Projects\D10_2; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\D10_2\*.dproj; DestDir: {app}\kitto\Examples\HelloKitto\Projects\D10_2; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\D10_2\*.res; DestDir: {app}\kitto\Examples\HelloKitto\Projects\D10_2; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Source\*; DestDir: {app}\kitto\Examples\HelloKitto\Source; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Readme.txt; DestDir: {app}\kitto\Examples\HelloKitto; Flags: ignoreversion; Components: HelloKittoExamples

Source: {#KITTO}\Examples\TasKitto\DB\*.sql; DestDir: {app}\kitto\Examples\TasKitto\DB; Flags: ignoreversion; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\DB\*.fbk; DestDir: {app}\kitto\Examples\TasKitto\DB; Flags: ignoreversion; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Home\TasKitto.exe; DestDir: {app}\kitto\Examples\TasKitto\Home; Flags: ignoreversion; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Home\TasKitto.kproj; DestDir: {app}\kitto\Examples\TasKitto\Home; Flags: ignoreversion; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Home\Metadata\*; DestDir: {app}\kitto\Examples\TasKitto\Home\Metadata; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Home\Resources\*; DestDir: {app}\kitto\Examples\TasKitto\Home\Resources; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Home\ReportTemplates\*; DestDir: {app}\kitto\Examples\TasKitto\Home\ReportTemplates; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\*.ico; DestDir: {app}\kitto\Examples\TasKitto\Projects; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\DXE7\*.dpr; DestDir: {app}\kitto\Examples\TasKitto\Projects\DXE7; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\DXE7\*.dproj; DestDir: {app}\kitto\Examples\TasKitto\Projects\DXE7; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\DXE7\*.res; DestDir: {app}\kitto\Examples\TasKitto\Projects\DXE7; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\DXE8\*.dpr; DestDir: {app}\kitto\Examples\TasKitto\Projects\DXE8; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\DXE8\*.dproj; DestDir: {app}\kitto\Examples\TasKitto\Projects\DXE8; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\DXE8\*.res; DestDir: {app}\kitto\Examples\TasKitto\Projects\DXE8; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\D10\*.dpr; DestDir: {app}\kitto\Examples\TasKitto\Projects\D10; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\D10\*.dproj; DestDir: {app}\kitto\Examples\TasKitto\Projects\D10; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\D10\*.res; DestDir: {app}\kitto\Examples\TasKitto\Projects\D10; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\D10_1\*.dpr; DestDir: {app}\kitto\Examples\TasKitto\Projects\D10_1; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\D10_1\*.dproj; DestDir: {app}\kitto\Examples\TasKitto\Projects\D10_1; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\D10_1\*.res; DestDir: {app}\kitto\Examples\TasKitto\Projects\D10_1; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\D10_2\*.dpr; DestDir: {app}\kitto\Examples\TasKitto\Projects\D10_2; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\D10_2\*.dproj; DestDir: {app}\kitto\Examples\TasKitto\Projects\D10_2; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\D10_2\*.res; DestDir: {app}\kitto\Examples\TasKitto\Projects\D10_2; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Source\*; DestDir: {app}\kitto\Examples\TasKitto\Source; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Readme.txt; DestDir: {app}\kitto\Examples\TasKitto; Flags: ignoreversion; Components: TasKittoExamples

Source: {#KITTO}\Examples\KEmployee\Home\KEmployee.kproj; DestDir: {app}\kitto\Examples\KEmployee\Home; Flags: ignoreversion; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Home\KEmployee.exe; DestDir: {app}\kitto\Examples\KEmployee\Home; Flags: ignoreversion; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Home\*.txt; DestDir: {app}\kitto\Examples\KEmployee\Home; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Home\Metadata\*; DestDir: {app}\kitto\Examples\KEmployee\Home\Metadata; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Home\Resources\*; DestDir: {app}\kitto\Examples\KEmployee\Home\Resources; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\*.ico; DestDir: {app}\kitto\Examples\KEmployee\Projects; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\DXE7\*.dpr; DestDir: {app}\kitto\Examples\KEmployee\Projects\DXE7; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\DXE7\*.dproj; DestDir: {app}\kitto\Examples\KEmployee\Projects\DXE7; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\DXE7\*.res; DestDir: {app}\kitto\Examples\KEmployee\Projects\DXE7; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\DXE8\*.dpr; DestDir: {app}\kitto\Examples\KEmployee\Projects\DXE8; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\DXE8\*.dproj; DestDir: {app}\kitto\Examples\KEmployee\Projects\DXE8; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\DXE8\*.res; DestDir: {app}\kitto\Examples\KEmployee\Projects\DXE8; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\D10\*.dpr; DestDir: {app}\kitto\Examples\KEmployee\Projects\D10; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\D10\*.dproj; DestDir: {app}\kitto\Examples\KEmployee\Projects\D10; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\D10\*.res; DestDir: {app}\kitto\Examples\KEmployee\Projects\D10; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\D10_1\*.dpr; DestDir: {app}\kitto\Examples\KEmployee\Projects\D10_1; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\D10_1\*.dproj; DestDir: {app}\kitto\Examples\KEmployee\Projects\D10_1; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\D10_1\*.res; DestDir: {app}\kitto\Examples\KEmployee\Projects\D10_1; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\D10_2\*.dpr; DestDir: {app}\kitto\Examples\KEmployee\Projects\D10_2; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\D10_2\*.dproj; DestDir: {app}\kitto\Examples\KEmployee\Projects\D10_2; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\D10_2\*.res; DestDir: {app}\kitto\Examples\KEmployee\Projects\D10_2; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Source\*; DestDir: {app}\kitto\Examples\KEmployee\Source; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Readme.txt; DestDir: {app}\kitto\Examples\KEmployee; Flags: ignoreversion; Components: KEmployeeExamples

Source: {#KITTO}\Examples\DbDemos\Home\DbDemos.kproj; DestDir: {app}\kitto\Examples\DbDemos\Home; Flags: ignoreversion; Components: DbDemosExamples
Source: {#KITTO}\Examples\DbDemos\Home\DbDemos.exe; DestDir: {app}\kitto\Examples\DbDemos\Home; Flags: ignoreversion; Components: DbDemosExamples
Source: {#KITTO}\Examples\DbDemos\Home\*.txt; DestDir: {app}\kitto\Examples\DbDemos\Home; Flags: ignoreversion recursesubdirs createallsubdirs; Components: DbDemosExamples
Source: {#KITTO}\Examples\DbDemos\Home\Metadata\*; DestDir: {app}\kitto\Examples\DbDemos\Home\Metadata; Flags: ignoreversion recursesubdirs createallsubdirs; Components: DbDemosExamples
Source: {#KITTO}\Examples\DbDemos\Home\Resources\*; DestDir: {app}\kitto\Examples\DbDemos\Home\Resources; Flags: ignoreversion recursesubdirs createallsubdirs; Components: DbDemosExamples
Source: {#KITTO}\Examples\DbDemos\Projects\*.ico; DestDir: {app}\kitto\Examples\DbDemos\Projects; Flags: ignoreversion recursesubdirs createallsubdirs; Components: DbDemosExamples
Source: {#KITTO}\Examples\DbDemos\Projects\D10_1\*.dpr; DestDir: {app}\kitto\Examples\DbDemos\Projects\D10_1; Flags: ignoreversion recursesubdirs createallsubdirs; Components: DbDemosExamples
Source: {#KITTO}\Examples\DbDemos\Projects\D10_1\*.dproj; DestDir: {app}\kitto\Examples\DbDemos\Projects\D10_1; Flags: ignoreversion recursesubdirs createallsubdirs; Components: DbDemosExamples
Source: {#KITTO}\Examples\DbDemos\Projects\D10_1\*.res; DestDir: {app}\kitto\Examples\DbDemos\Projects\D10_1; Flags: ignoreversion recursesubdirs createallsubdirs; Components: DbDemosExamples
Source: {#KITTO}\Examples\DbDemos\Projects\D10_2\*.dpr; DestDir: {app}\kitto\Examples\DbDemos\Projects\D10_2; Flags: ignoreversion recursesubdirs createallsubdirs; Components: DbDemosExamples
Source: {#KITTO}\Examples\DbDemos\Projects\D10_2\*.dproj; DestDir: {app}\kitto\Examples\DbDemos\Projects\D10_2; Flags: ignoreversion recursesubdirs createallsubdirs; Components: DbDemosExamples
Source: {#KITTO}\Examples\DbDemos\Projects\D10_2\*.res; DestDir: {app}\kitto\Examples\DbDemos\Projects\D10_2; Flags: ignoreversion recursesubdirs createallsubdirs; Components: DbDemosExamples
Source: {#KITTO}\Examples\DbDemos\Source\*; DestDir: {app}\kitto\Examples\DbDemos\Source; Flags: ignoreversion recursesubdirs createallsubdirs; Components: DbDemosExamples
Source: {#KITTO}\Examples\DbDemos\Readme.txt; DestDir: {app}\kitto\Examples\DbDemos; Flags: ignoreversion; Components: DbDemosExamples

[Dirs]
Name: "{app}"; Permissions: users-full

[Icons]
Name: {group}\Kitto2; Filename: {app}\kitto\Kide\Bin\{#MyAppExeName}; Components: Kide2
Name: {commondesktop}\Kide2; Filename: {app}\kitto\Kide\Bin\{#MyAppExeName}; Tasks: desktopicon; Components: Kide2
Name: {userappdata}\Microsoft\Internet Explorer\Quick Launch\Kide2; Filename: {app}\kitto\Kide\Bin\{#MyAppExeName}; Tasks: quicklaunchicon; Components: Kide2

Name: {app}\kitto\Examples\HelloKitto\Home\HelloKitto - Install as service; Filename: {app}\kitto\Examples\HelloKitto\Home\HelloKitto.exe; Parameters: -install; Components: HelloKittoExamples
Name: {app}\kitto\Examples\HelloKitto\Home\HelloKitto - Uninstall service; Filename: {app}\kitto\Examples\HelloKitto\Home\HelloKitto.exe; Parameters: -uninstall; Components: HelloKittoExamples
Name: {app}\kitto\Examples\HelloKitto\Home\HelloKitto - Run as application; Filename: {app}\kitto\Examples\HelloKitto\Home\HelloKitto.exe; Parameters: -a; Components: HelloKittoExamples
Name: {app}\kitto\Examples\TasKitto\Home\TasKitto - Install as service; Filename: {app}\kitto\Examples\TasKitto\Home\TasKitto.exe; Parameters: -install; Components: TasKittoExamples
Name: {app}\kitto\Examples\TasKitto\Home\TasKitto - Uninstall service; Filename: {app}\kitto\Examples\TasKitto\Home\TasKitto.exe; Parameters: -uninstall; Components: TasKittoExamples
Name: {app}\kitto\Examples\TasKitto\Home\TasKitto - Run as application; Filename: {app}\kitto\Examples\TasKitto\Home\TasKitto.exe; Parameters: -a; Components: TasKittoExamples
Name: {app}\kitto\Examples\KEmployee\Home\KEmployee - Install as service; Filename: {app}\kitto\Examples\KEmployee\Home\KEmployee.exe; Parameters: -install; Components: KEmployeeExamples
Name: {app}\kitto\Examples\KEmployee\Home\KEmployee - Uninstall service; Filename: {app}\kitto\Examples\KEmployee\Home\KEmployee.exe; Parameters: -uninstall; Components: KEmployeeExamples
Name: {app}\kitto\Examples\KEmployee\Home\KEmployee - Run as application; Filename: {app}\kitto\Examples\KEmployee\Home\KEmployee.exe; Parameters: -a; Components: KEmployeeExamples
Name: {app}\kitto\Examples\DbDemos\Home\DbDemos - Install as service; Filename: {app}\kitto\Examples\DbDemos\Home\DbDemos.exe; Parameters: -install; Components: DbDemosExamples
Name: {app}\kitto\Examples\DbDemos\Home\DbDemos - Uninstall service; Filename: {app}\kitto\Examples\DbDemos\Home\DbDemos.exe; Parameters: -uninstall; Components: DbDemosExamples
Name: {app}\kitto\Examples\DbDemos\Home\DbDemos - Run as application; Filename: {app}\kitto\Examples\DbDemos\Home\DbDemos.exe; Parameters: -a; Components: DbDemosExamples

[Run]
Filename: {app}\kitto\Kide\Bin\{#MyAppExeName}; Description: {cm:LaunchProgram, Kide}; Flags: nowait postinstall skipifsilent

[Components]
Name: Kitto2; Description: Kitto2 source code; Types: full compact
Name: Kide2; Description: KIDE2 - Kitto2 IDE; Types: custom compact full
Name: HelloKittoExamples; Description: HelloKitto example; Types: custom full
Name: TasKittoExamples; Description: TasKitto example; Types: custom full
Name: KEmployeeExamples; Description: KEmployee example; Types: custom full
Name: DbDemosExamples; Description: DbDemos example; Types: custom full
Name: Kitto2Docs; Description: Kitto2 reference documentation; Types: custom full

[Registry]
;kproj extension opened by KIDE
Root: HKCR; SubKey: .kproj; ValueType: string; ValueData: Open; Flags: uninsdeletekey;
Root: HKCR; SubKey: Open; ValueType: string; ValueData: Kide project file; Flags: uninsdeletekey;
Root: HKCR; SubKey: Open\Shell\Open\Command; ValueType: string; ValueData: """{app}\kitto\Kide\Bin\{#MyAppExeName}"" ""%1"""; Flags: uninsdeletevalue;
Root: HKCR; Subkey: Open\DefaultIcon; ValueType: string; ValueData: {app}\kitto\Kide\Bin\{#MyAppExeName},0; Flags: uninsdeletevalue;

[UninstallDelete]
Name: {app}; Type: filesandordirs
