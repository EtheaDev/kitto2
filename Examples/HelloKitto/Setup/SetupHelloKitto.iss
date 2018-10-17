; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#define MyAppName "HelloKitto"
#define MyAppVersion "2.0"
#define MyAppPublisher "Ethea S.r.l."
#define MyAppURL "http://www.ethea.it/"
#define MyAppExeName "HelloKitto.exe"

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{97C81C6A-4495-4F63-A12F-A2CD09EFEA2A}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
;AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName=C:\Kitto2Apps\{#MyAppName}
DefaultGroupName={#MyAppName}
OutputBaseFilename=SetupHelloKitto
Compression=lzma
SolidCompression=yes
WizardImageFile=WizKittoImage.bmp
WizardSmallImageFile=WizKittoSmallImage.bmp
AppCopyright=CopyRight (c) Ethea S.r.l.
SetupIconFile=Kitto_64.ico
VersionInfoCompany=Ethea S.r.l.
VersionInfoCopyright=CopyRight (c) Ethea S.r.l.
DisableWelcomePage=False
UsePreviousAppDir=False

[Languages]
Name: "italian"; MessagesFile: "compiler:Languages\Italian.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
; Kitto Core Resources
Source: "..\..\..\Home\Resources\*"; DestDir: "{app}\Home\Resources"; Flags: ignoreversion recursesubdirs createallsubdirs 
Source: "..\..\..\Home\Locale\*"; DestDir: "{app}\Home\Locale"; Flags: ignoreversion recursesubdirs createallsubdirs 

; Application files
Source: "..\Home\HelloKitto.exe"; DestDir: "{app}\Home"; Flags: ignoreversion
Source: "*.cmd"; DestDir: "{app}\Home"; Flags: ignoreversion
Source: "..\Home\Locale\*"; DestDir: "{app}\Home\Locale"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\Home\Metadata\Models\*"; DestDir: "{app}\Home\Metadata\Models"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\Home\Metadata\Views\*"; DestDir: "{app}\Home\Metadata\Views"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\Home\ReportTemplates\*"; DestDir: "{app}\Home\ReportTemplates"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\Home\Resources\*"; DestDir: "{app}\Home\Resources"; Flags: ignoreversion recursesubdirs createallsubdirs

;Custom Config copy for deployment: comparetimestamp so don't replace if file was changed directly on web-server
Source: "Config.yaml"; DestDir: "{app}\Home\Metadata"; Flags: ignoreversion comparetimestamp uninsneveruninstall; Permissions: users-modify

; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Dirs]
;Directtory for persistent output files
;Name: "{app}\Home\UploadedFiles\"; Permissions: users-full

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\Home\{#MyAppExeName}"; Parameters: "-a"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\Home\{#MyAppExeName}"; Parameters: "-a"; Tasks: desktopicon

[Run]
Filename: "{app}\Home\{#MyAppExeName}"; Parameters: "-a"; Flags: nowait postinstall skipifsilent; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"
