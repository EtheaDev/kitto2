#define MyAppName "Kitto2 and Kide2 IDE"

#define KittoVersion "2.0"
#define MyAppVersion "2.0.0"
#define MyAppExeName "Kide2.exe"
#define MyCopyRight "Copyright (c) 2012-2017 Ethea S.r.l."
#define MyCompany "Ethea S.r.l."

#define KITTO "..\..\..\Kitto"

[Setup]
AppId={{052FA381-75D0-4B82-B6AD-FB9EF1967663}
DefaultDirName={sd}\Dev\Kitto2Ent_{#KittoVersion}
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
Source: ..\Bin\Kide2.exe; DestDir: {app}\Kide2\Bin; Flags: ignoreversion; Components: Kide2
Source: ..\Bin\Config.yaml; DestDir: {app}\Kide2\Bin; Flags: ignoreversion; Components: Kide2
Source: ..\Bin\logo.png; DestDir: {app}\Kide2\Bin; Flags: ignoreversion; Components: Kide2
Source: ..\Bin\background.jpg; DestDir: {app}\Kide2\Bin; Flags: ignoreversion; Components: Kide2
Source: ..\Bin\ProjectTemplates\*; DestDir: {app}\Kide2\Bin\ProjectTemplates; Flags: ignoreversion recursesubdirs createallsubdirs; Components: Kide2
Source: ..\Bin\MetadataTemplates\*; DestDir: {app}\Kide2\Bin\MetadataTemplates; Flags: ignoreversion recursesubdirs createallsubdirs; Components: Kide2

Source: {#KITTO}\Source\*; DestDir: {app}\Kitto\Source; Flags: ignoreversion recursesubdirs createallsubdirs; Components: Kitto2
;Source: {#KITTO}\Bin\*.yaml; DestDir: {app}\Kitto\Bin; Flags: ignoreversion; Components: Kitto2
Source: {#KITTO}\Docs\External\*.txt; DestDir: {app}\Kitto\Docs\External; Flags: ignoreversion; Components: Kitto2
Source: {#KITTO}\Home\*; DestDir: {app}\Kitto\Home; Flags: ignoreversion recursesubdirs createallsubdirs; Components: Kitto2
Source: .\MyProjectsFolder.txt; DestDir: {app}\Kitto\MyProjects; Flags: ignoreversion; Components: Kitto2

;Source: {#KITTO}\Docs\External\Reference\Html\*; DestDir: {app}\Kitto\Docs\Reference; Flags: ignoreversion recursesubdirs createallsubdirs; Components: Kitto2Docs
Source: {#KITTO}\Docs\External\*.png; DestDir: {app}\Kitto\Docs; Flags: ignoreversion; Components: Kitto2Docs

Source: {#KITTO}\Examples\HelloKitto\DB\*.sql; DestDir: {app}\Kitto2\Examples\HelloKitto\DB; Flags: ignoreversion; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\DB\*.fbk; DestDir: {app}\Kitto2\Examples\HelloKitto\DB; Flags: ignoreversion; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Home\HelloKitto.exe; DestDir: {app}\Kitto2\Examples\HelloKitto\Home; Flags: ignoreversion; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Home\HelloKitto.kproj; DestDir: {app}\Kitto2\Examples\HelloKitto\Home; Flags: ignoreversion; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Home\Locale\*; DestDir: {app}\Kitto2\Examples\HelloKitto\Home\Locale; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Home\Metadata\*; DestDir: {app}\Kitto2\Examples\HelloKitto\Home\Metadata; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Home\Resources\*; DestDir: {app}\Kitto2\Examples\HelloKitto\Home\Resources; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Home\ReportTemplates\*; DestDir: {app}\Kitto2\Examples\HelloKitto\Home\ReportTemplates; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\*.ico; DestDir: {app}\Kitto2\Examples\HelloKitto\Projects; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\DXE7\*.dpr; DestDir: {app}\Kitto2\Examples\HelloKitto\Projects\DXE7; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\DXE7\*.dproj; DestDir: {app}\Kitto2\Examples\HelloKitto\Projects\DXE7; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\DXE7\*.res; DestDir: {app}\Kitto2\Examples\HelloKitto\Projects\DXE7; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\DXE8\*.dpr; DestDir: {app}\Kitto2\Examples\HelloKitto\Projects\DXE8; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\DXE8\*.dproj; DestDir: {app}\Kitto2\Examples\HelloKitto\Projects\DXE8; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\DXE8\*.res; DestDir: {app}\Kitto2\Examples\HelloKitto\Projects\DXE8; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\D10\*.dpr; DestDir: {app}\Kitto2\Examples\HelloKitto\Projects\D10; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\D10\*.dproj; DestDir: {app}\Kitto2\Examples\HelloKitto\Projects\D10; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\D10\*.res; DestDir: {app}\Kitto2\Examples\HelloKitto\Projects\D10; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\D10_1\*.dpr; DestDir: {app}\Kitto2\Examples\HelloKitto\Projects\D10_1; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\D10_1\*.dproj; DestDir: {app}\Kitto2\Examples\HelloKitto\Projects\D10_1; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\D10_1\*.res; DestDir: {app}\Kitto2\Examples\HelloKitto\Projects\D10_1; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\D10_2\*.dpr; DestDir: {app}\Kitto2\Examples\HelloKitto\Projects\D10_2; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\D10_2\*.dproj; DestDir: {app}\Kitto2\Examples\HelloKitto\Projects\D10_2; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Projects\D10_2\*.res; DestDir: {app}\Kitto2\Examples\HelloKitto\Projects\D10_2; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Source\*; DestDir: {app}\Kitto2\Examples\HelloKitto\Source; Flags: ignoreversion recursesubdirs createallsubdirs; Components: HelloKittoExamples
Source: {#KITTO}\Examples\HelloKitto\Readme.txt; DestDir: {app}\Kitto2\Examples\HelloKitto; Flags: ignoreversion; Components: HelloKittoExamples

Source: {#KITTO}\Examples\TasKitto\DB\*.sql; DestDir: {app}\Kitto2\Examples\TasKitto\DB; Flags: ignoreversion; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\DB\*.fbk; DestDir: {app}\Kitto2\Examples\TasKitto\DB; Flags: ignoreversion; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Home\TasKitto.exe; DestDir: {app}\Kitto2\Examples\TasKitto\Home; Flags: ignoreversion; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Home\TasKitto.kproj; DestDir: {app}\Kitto2\Examples\TasKitto\Home; Flags: ignoreversion; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Home\Metadata\*; DestDir: {app}\Kitto2\Examples\TasKitto\Home\Metadata; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Home\Resources\*; DestDir: {app}\Kitto2\Examples\TasKitto\Home\Resources; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Home\ReportTemplates\*; DestDir: {app}\Kitto2\Examples\TasKitto\Home\ReportTemplates; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\*.ico; DestDir: {app}\Kitto2\Examples\TasKitto\Projects; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\DXE7\*.dpr; DestDir: {app}\Kitto2\Examples\TasKitto\Projects\DXE7; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\DXE7\*.dproj; DestDir: {app}\Kitto2\Examples\TasKitto\Projects\DXE7; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\DXE7\*.res; DestDir: {app}\Kitto2\Examples\TasKitto\Projects\DXE7; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\DXE8\*.dpr; DestDir: {app}\Kitto2\Examples\TasKitto\Projects\DXE8; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\DXE8\*.dproj; DestDir: {app}\Kitto2\Examples\TasKitto\Projects\DXE8; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\DXE8\*.res; DestDir: {app}\Kitto2\Examples\TasKitto\Projects\DXE8; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\D10\*.dpr; DestDir: {app}\Kitto2\Examples\TasKitto\Projects\D10; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\D10\*.dproj; DestDir: {app}\Kitto2\Examples\TasKitto\Projects\D10; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\D10\*.res; DestDir: {app}\Kitto2\Examples\TasKitto\Projects\D10; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\D10_1\*.dpr; DestDir: {app}\Kitto2\Examples\TasKitto\Projects\D10_1; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\D10_1\*.dproj; DestDir: {app}\Kitto2\Examples\TasKitto\Projects\D10_1; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\D10_1\*.res; DestDir: {app}\Kitto2\Examples\TasKitto\Projects\D10_1; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\D10_2\*.dpr; DestDir: {app}\Kitto2\Examples\TasKitto\Projects\D10_2; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\D10_2\*.dproj; DestDir: {app}\Kitto2\Examples\TasKitto\Projects\D10_2; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Projects\D10_2\*.res; DestDir: {app}\Kitto2\Examples\TasKitto\Projects\D10_2; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Source\*; DestDir: {app}\Kitto2\Examples\TasKitto\Source; Flags: ignoreversion recursesubdirs createallsubdirs; Components: TasKittoExamples
Source: {#KITTO}\Examples\TasKitto\Readme.txt; DestDir: {app}\Kitto2\Examples\TasKitto; Flags: ignoreversion; Components: TasKittoExamples

Source: {#KITTO}\Examples\KEmployee\Home\KEmployee.kproj; DestDir: {app}\Kitto2\Examples\KEmployee\Home; Flags: ignoreversion; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Home\KEmployee.exe; DestDir: {app}\Kitto2\Examples\KEmployee\Home; Flags: ignoreversion; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Home\*.txt; DestDir: {app}\Kitto2\Examples\KEmployee\Home; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Home\Metadata\*; DestDir: {app}\Kitto2\Examples\KEmployee\Home\Metadata; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Home\Resources\*; DestDir: {app}\Kitto2\Examples\KEmployee\Home\Resources; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\*.ico; DestDir: {app}\Kitto2\Examples\KEmployee\Projects; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\DXE7\*.dpr; DestDir: {app}\Kitto2\Examples\KEmployee\Projects\DXE7; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\DXE7\*.dproj; DestDir: {app}\Kitto2\Examples\KEmployee\Projects\DXE7; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\DXE7\*.res; DestDir: {app}\Kitto2\Examples\KEmployee\Projects\DXE7; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\DXE8\*.dpr; DestDir: {app}\Kitto2\Examples\KEmployee\Projects\DXE8; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\DXE8\*.dproj; DestDir: {app}\Kitto2\Examples\KEmployee\Projects\DXE8; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\DXE8\*.res; DestDir: {app}\Kitto2\Examples\KEmployee\Projects\DXE8; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\D10\*.dpr; DestDir: {app}\Kitto2\Examples\KEmployee\Projects\D10; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\D10\*.dproj; DestDir: {app}\Kitto2\Examples\KEmployee\Projects\D10; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\D10\*.res; DestDir: {app}\Kitto2\Examples\KEmployee\Projects\D10; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\D10_1\*.dpr; DestDir: {app}\Kitto2\Examples\KEmployee\Projects\D10_1; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\D10_1\*.dproj; DestDir: {app}\Kitto2\Examples\KEmployee\Projects\D10_1; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\D10_1\*.res; DestDir: {app}\Kitto2\Examples\KEmployee\Projects\D10_1; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\D10_2\*.dpr; DestDir: {app}\Kitto2\Examples\KEmployee\Projects\D10_2; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\D10_2\*.dproj; DestDir: {app}\Kitto2\Examples\KEmployee\Projects\D10_2; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Projects\D10_2\*.res; DestDir: {app}\Kitto2\Examples\KEmployee\Projects\D10_2; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Source\*; DestDir: {app}\Kitto2\Examples\KEmployee\Source; Flags: ignoreversion recursesubdirs createallsubdirs; Components: KEmployeeExamples
Source: {#KITTO}\Examples\KEmployee\Readme.txt; DestDir: {app}\Kitto2\Examples\KEmployee; Flags: ignoreversion; Components: KEmployeeExamples


[Dirs]
Name: "{app}"; Permissions: users-full

[Icons]
Name: {group}\Kitto2; Filename: {app}\Kide2\Bin\{#MyAppExeName}; Components: Kide2
Name: {commondesktop}\Kide2; Filename: {app}\Kide2\Bin\{#MyAppExeName}; Tasks: desktopicon; Components: Kide2
Name: {userappdata}\Microsoft\Internet Explorer\Quick Launch\Kide2; Filename: {app}\Kide2\Bin\{#MyAppExeName}; Tasks: quicklaunchicon; Components: Kide2

Name: {app}\Kitto2\Examples\HelloKitto\Home\HelloKitto - Install as service; Filename: {app}\Kitto2\Examples\HelloKitto\Home\HelloKitto.exe; Parameters: -install; Components: HelloKittoExamples
Name: {app}\Kitto2\Examples\HelloKitto\Home\HelloKitto - Uninstall service; Filename: {app}\Kitto2\Examples\HelloKitto\Home\HelloKitto.exe; Parameters: -uninstall; Components: HelloKittoExamples
Name: {app}\Kitto2\Examples\HelloKitto\Home\HelloKitto - Run as application; Filename: {app}\Kitto2\Examples\HelloKitto\Home\HelloKitto.exe; Parameters: -a; Components: HelloKittoExamples
Name: {app}\Kitto2\Examples\TasKitto\Home\TasKitto - Install as service; Filename: {app}\Kitto2\Examples\TasKitto\Home\TasKitto.exe; Parameters: -install; Components: TasKittoExamples
Name: {app}\Kitto2\Examples\TasKitto\Home\TasKitto - Uninstall service; Filename: {app}\Kitto2\Examples\TasKitto\Home\TasKitto.exe; Parameters: -uninstall; Components: TasKittoExamples
Name: {app}\Kitto2\Examples\TasKitto\Home\TasKitto - Run as application; Filename: {app}\Kitto2\Examples\TasKitto\Home\TasKitto.exe; Parameters: -a; Components: TasKittoExamples
Name: {app}\Kitto2\Examples\KEmployee\Home\KEmployee - Install as service; Filename: {app}\Kitto2\Examples\KEmployee\Home\KEmployee.exe; Parameters: -install; Components: KEmployeeExamples
Name: {app}\Kitto2\Examples\KEmployee\Home\KEmployee - Uninstall service; Filename: {app}\Kitto2\Examples\KEmployee\Home\KEmployee.exe; Parameters: -uninstall; Components: KEmployeeExamples
Name: {app}\Kitto2\Examples\KEmployee\Home\KEmployee - Run as application; Filename: {app}\Kitto2\Examples\KEmployee\Home\KEmployee.exe; Parameters: -a; Components: KEmployeeExamples

[Run]
Filename: {app}\Kide2\Bin\{#MyAppExeName}; Description: {cm:LaunchProgram, Kide}; Flags: nowait postinstall skipifsilent

[Components]
Name: Kitto2; Description: Kitto2 source code; Types: full compact
Name: Kide2; Description: KIDE2 - Kitto2 IDE; Types: custom compact full
Name: HelloKittoExamples; Description: HelloKitto example; Types: custom full
Name: TasKittoExamples; Description: TasKitto example; Types: custom full
Name: KEmployeeExamples; Description: KEmployee example; Types: custom full
Name: Kitto2Docs; Description: Kitto2 reference documentation; Types: custom full

[Registry]
;kproj extension opened by KIDE
Root: HKCR; SubKey: .kproj; ValueType: string; ValueData: Open; Flags: uninsdeletekey;
Root: HKCR; SubKey: Open; ValueType: string; ValueData: Kide project file; Flags: uninsdeletekey;
Root: HKCR; SubKey: Open\Shell\Open\Command; ValueType: string; ValueData: """{app}\Kide2\Bin\{#MyAppExeName}"" ""%1"""; Flags: uninsdeletevalue;
Root: HKCR; Subkey: Open\DefaultIcon; ValueType: string; ValueData: {app}\Kide2\Bin\{#MyAppExeName},0; Flags: uninsdeletevalue;

[UninstallDelete]
Name: {app}; Type: filesandordirs
