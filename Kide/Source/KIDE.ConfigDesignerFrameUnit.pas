{*******************************************************************}
{                                                                   }
{   Kide2 Editor: GUI for Kitto2                                    }
{                                                                   }
{   Copyright (c) 2012-2017 Ethea S.r.l.                            }
{   ALL RIGHTS RESERVED / TUTTI I DIRITTI RISERVATI                 }
{                                                                   }
{*******************************************************************}
{                                                                   }
{   The entire contents of this file is protected by                }
{   International Copyright Laws. Unauthorized reproduction,        }
{   reverse-engineering, and distribution of all or any portion of  }
{   the code contained in this file is strictly prohibited and may  }
{   result in severe civil and criminal penalties and will be       }
{   prosecuted to the maximum extent possible under the law.        }
{                                                                   }
{   RESTRICTIONS                                                    }
{                                                                   }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED      }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE        }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE       }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT  }
{   AND PERMISSION FROM ETHEA S.R.L.                                }
{                                                                   }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON       }
{   ADDITIONAL RESTRICTIONS.                                        }
{                                                                   }
{*******************************************************************}
{                                                                   }
{   Il contenuto di questo file è protetto dalle leggi              }
{   internazionali sul Copyright. Sono vietate la riproduzione, il  }
{   reverse-engineering e la distribuzione non autorizzate di tutto }
{   o parte del codice contenuto in questo file. Ogni infrazione    }
{   sarà perseguita civilmente e penalmente a termini di legge.     }
{                                                                   }
{   RESTRIZIONI                                                     }
{                                                                   }
{   SONO VIETATE, SENZA IL CONSENSO SCRITTO DA PARTE DI             }
{   ETHEA S.R.L., LA COPIA, LA VENDITA, LA DISTRIBUZIONE E IL       }
{   TRASFERIMENTO A TERZI, A QUALUNQUE TITOLO, DEL CODICE SORGENTE  }
{   CONTENUTO IN QUESTO FILE E ALTRI FILE AD ESSO COLLEGATI.        }
{                                                                   }
{   SI FACCIA RIFERIMENTO ALLA LICENZA D'USO PER INFORMAZIONI SU    }
{   EVENTUALI RESTRIZIONI ULTERIORI.                                }
{                                                                   }
{*******************************************************************}
unit KIDE.ConfigDesignerFrameUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, KIDE.TreeDesignerFrameUnit, Vcl.Samples.Spin,
  KIDE.BaseFrameUnit, KIDE.CodeEditorFrameUnit, Vcl.ExtCtrls, Vcl.Tabs,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls,
  EF.Tree, KIDE.NodeDesignerFrameUnit,
  KIDE.EditNodeBaseFrameUnit;

type
  TConfigDesignerFrame = class(TTreeDesignerFrame)
    EditorPageControl: TPageControl;
    MainTabSheet: TTabSheet;
    DatabasesTabSheet: TTabSheet;
    AuthTabSheet: TTabSheet;
    LogTabSheet: TTabSheet;
    TextFileGroupBox: TGroupBox;
    _Log_TextFile_FileName: TLabeledEdit;
    LogPanel: TPanel;
    LevelLabel: TLabel;
    _Log_Level: TSpinEdit;
    AccessControlTabSheet: TTabSheet;
    ControllerScrollBox: TScrollBox;
    ControllerAutoscrollPanel: TPanel;
    ApplicationGroupBox: TGroupBox;
    LanguageIdLabel: TLabel;
    CharSetLabel: TLabel;
    _AppTitle: TLabeledEdit;
    LanguageIdComboBox: TComboBox;
    CharSetComboBox: TComboBox;
    _HomeView: TLabeledEdit;
    UserFormatsGroupBox: TGroupBox;
    _UserFormats_Time: TLabeledEdit;
    _UserFormats_Date: TLabeledEdit;
    FastCGIGroupBox: TGroupBox;
    SessionTimeoutLabel: TLabel;
    TCPPortLabel: TLabel;
    _FastCGI_TCPPort: TSpinEdit;
    _FastCGI_SessionTimeout: TSpinEdit;
    ExtGroupBox: TGroupBox;
    ThemeLabel: TLabel;
    AjaxTimeoutLabel: TLabel;
    _Ext_URL: TLabeledEdit;
    _Ext_Theme: TComboBox;
    _Ext_AjaxTimeout: TSpinEdit;
    _FOPEnginePath: TLabeledEdit;
    _JavascriptLibraries: TLabeledEdit;
    _LanguagePerSession: TCheckBox;
    TopPanel: TPanel;
    _DatabaseRouter: TLabeledEdit;
    _DefaultDatabaseName: TLabeledEdit;
    DatabasesPanel: TPanel;
    DatabasesLabel: TLabel;
    EmailTabSheet: TTabSheet;
    LoginTabSheet: TTabSheet;
    _Log_TextFile_IsEnabled: TCheckBox;
    DefaultsTabSheet: TTabSheet;
    procedure EditorPageControlChange(Sender: TObject);
  private
  protected
    class function SuitsTree(const ATree: TEFTree): Boolean; override;
    procedure CleanupDefaultsToEditNode; override;
    procedure UpdateDesignPanel(const AForce: Boolean); override;
    procedure DesignPanelToEditNode; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Init(const ANode: TEFTree); override;
  end;

implementation

{$R *.dfm}

uses
  KIDE.Config,
  EF.Classes, KIDE.Utils,
  Kitto.Config,
  //Embedded forms
  KIDE.ConfigAuthNodeFrameUnit, KIDE.ConfigDatabasesNodeFrameUnit,
  KIDE.ConfigAccessControlNodeFrameUnit, KIDE.ConfigEmailNodeFrameUnit,
  KIDE.LoginViewDesignerFrameUnit, KIDE.ConfigDefaultsNodeFrameUnit;

{ TConfigDesignerFrame }

procedure TConfigDesignerFrame.CleanupDefaultsToEditNode;
begin
  inherited;
  CleanupTextNode('JavascriptLibraries');
  CleanupTextNode('UserFormats/Time');
  CleanupTextNode('UserFormats/Date');
  CleanupOrphanNode('UserFormats');
  CleanupIntegerNode('FastCGI/TCPPort', 2014);
  CleanupIntegerNode('FastCGI/SessionTimeout', 30);
  CleanupTextNode('Ext/Theme', 'default');
  CleanupTextNode('Ext/URL', '/ext');
  CleanupIntegerNode('Ext/AjaxTimeout', 30000);
  CleanupOrphanNode('FastCGI');
  CleanupBooleanNode('Log/TextFile/IsEnabled');
  CleanupIntegerNode('Log/Level');
  CleanupTextNode('Log/TextFile/FileName');
  CleanupOrphanNode('Log/TextFile');
  CleanupOrphanNode('Log');
  CleanupTextNode('FOPEnginePath');
  CleanupTextNode('HomeView', 'Home');
  CleanupTextNode('DefaultDatabaseName', 'Main');
  CleanupTextNode('DatabaseRouter');
  CleanupBooleanNode('LanguagePerSession', False);
  CleanupTextNode('LanguageId');
  CleanupOrphanNode('Auth');
  CleanupOrphanNode('Databases');
  CleanupOrphanNode('AccessControl');
  CleanupOrphanNode('Email/SMTP/Default');
  CleanupOrphanNode('Email/SMTP');
  CleanupOrphanNode('Email');
  CleanupOrphanNode('Login');
  CleanupOrphanNode('Defaults/Grid');
  CleanupOrphanNode('Defaults/Window');
  CleanupOrphanNode('Defaults');
end;

constructor TConfigDesignerFrame.Create(AOwner: TComponent);
begin
  inherited;
  TKideConfig.Instance.Config.GetNode('ExtThemes').GetChildValues(_Ext_Theme.Items);
  TKideConfig.Instance.Config.GetNode('LanguageIds').GetChildValues(LanguageIdComboBox.Items);
  TKideConfig.Instance.Config.GetNode('Charsets').GetChildValues(CharsetComboBox.Items);
  //TConfig.Instance.Config.GetNode('KittoSearchPaths').GetChildValues(SearchPathComboBox.Items);
end;

procedure TConfigDesignerFrame.DesignPanelToEditNode;
begin
  inherited;
  EditorPageControl.ActivePageIndex := 0;
  EditNode.SetString('LanguageId', Copy(LanguageIdComboBox.Text,1,2));
  EditNode.SetString('Charset' ,CharSetComboBox.Text);
end;

procedure TConfigDesignerFrame.EditorPageControlChange(Sender: TObject);
begin
  inherited;
  if EditorPageControl.ActivePage = AuthTabSheet then
    EmbedEditNodeFrame(AuthTabSheet, TConfigAuthNodeFrame,
      EditNode.GetNode('Auth', True), True)
  else if EditorPageControl.ActivePage = DatabasesTabSheet then
    EmbedEditNodeFrame(DatabasesPanel, TConfigDatabasesNodeFrame,
      EditNode.GetNode('Databases', True), True)
  else if EditorPageControl.ActivePage = AccessControlTabSheet then
    EmbedEditNodeFrame(AccessControlTabSheet, TConfigAccessControlNodeFrame,
      EditNode.GetNode('AccessControl', True), True)
  else if EditorPageControl.ActivePage = LoginTabSheet then
    EmbedEditNodeFrame(LoginTabSheet, TLoginViewDesignerFrame,
      EditNode.GetNode('Login', True), True)
  else if EditorPageControl.ActivePage = EmailTabSheet then
    EmbedEditNodeFrame(EmailTabSheet, TConfigEmailNodeFrame,
      EditNode.FindNode('Email', True), True)
  else if EditorPageControl.ActivePage = DefaultsTabSheet then
    EmbedEditNodeFrame(DefaultsTabSheet, TConfigDefaultsNodeFrame,
      EditNode.FindNode('Defaults', True), True);
end;

procedure TConfigDesignerFrame.Init(const ANode: TEFTree);
begin
  inherited;
end;

class function TConfigDesignerFrame.SuitsTree(const ATree: TEFTree): Boolean;
begin
  Result := ATree is TEFComponentConfig;
end;

procedure TConfigDesignerFrame.UpdateDesignPanel(const AForce: Boolean);
var
  LLocaleId: string;
  I: integer;
  LCharSet: string;
  LTheme: string;
begin
  inherited;
  EditorPageControl.ActivePageIndex := 0;

  _FastCGI_TCPPort.Value := EditNode.GetInteger('FastCGI/TCPPort', 2014);
  _FastCGI_SessionTimeout.Value := EditNode.GetInteger('FastCGI/SessionTimeout', 30);

  LTheme := EditNode.GetString('Ext/Theme', 'default');
  _Ext_Theme.ItemIndex := _Ext_Theme.Items.IndexOf(LTheme);
  _Ext_URL.Text := EditNode.GetString('Ext/URL', '/ext');
  _Ext_AjaxTimeout.Value := EditNode.GetInteger('Ext/AjaxTimeout', 30000);
  _HomeView.Text := EditNode.GetString('HomeView', 'Home');

  _DefaultDatabaseName.Text := EditNode.GetString('DefaultDatabaseName', 'Main');
  //Align ComboBox item
  LLocaleId := EditNode.GetString('LanguageId', 'en');
  for I := 0 to LanguageIdComboBox.Items.Count -1 do
  begin
    if SameText(Copy(LanguageIdComboBox.Items[I],1,2), LLocaleId) then
    begin
      LanguageIdComboBox.ItemIndex := I;
      break;
    end;
  end;
  LCharSet := EditNode.GetString('Charset');
  CharSetComboBox.ItemIndex := CharSetComboBox.Items.IndexOf(LCharSet);
end;

initialization
  TTreeDesignerFrameRegistry.Instance.RegisterClass(TConfigDesignerFrame.GetClassId, TConfigDesignerFrame);

finalization
  TTreeDesignerFrameRegistry.Instance.UnregisterClass(TConfigDesignerFrame.GetClassId);

end.
