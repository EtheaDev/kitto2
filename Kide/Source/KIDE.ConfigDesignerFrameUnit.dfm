inherited ConfigDesignerFrame: TConfigDesignerFrame
  Width = 655
  Height = 548
  HelpKeyword = 'ConfigFile'
  ExplicitWidth = 655
  ExplicitHeight = 548
  inherited ClientPanel: TPanel
    Width = 634
    Height = 548
    ExplicitWidth = 634
    ExplicitHeight = 548
    inherited DesignPanel: TPanel
      Width = 634
      Height = 491
      ExplicitWidth = 634
      ExplicitHeight = 491
      inherited ToolBar: TToolBar
        Width = 634
        TabOrder = 1
        ExplicitWidth = 634
      end
      object EditorPageControl: TPageControl
        Left = 0
        Top = 26
        Width = 634
        Height = 465
        HelpType = htKeyword
        HelpKeyword = 'ConfigFile'
        ActivePage = MainTabSheet
        Align = alClient
        TabOrder = 0
        OnChange = EditorPageControlChange
        object MainTabSheet: TTabSheet
          Caption = 'Configuration'
          object ControllerScrollBox: TScrollBox
            Left = 0
            Top = 0
            Width = 626
            Height = 437
            Align = alClient
            BevelInner = bvNone
            BevelOuter = bvNone
            BorderStyle = bsNone
            TabOrder = 0
            object ControllerAutoscrollPanel: TPanel
              Left = 0
              Top = 362
              Width = 401
              Height = 75
              Align = alLeft
              BevelOuter = bvNone
              TabOrder = 0
            end
            object ApplicationGroupBox: TGroupBox
              Left = 0
              Top = 0
              Width = 626
              Height = 185
              Align = alTop
              Caption = 'Application'
              TabOrder = 1
              DesignSize = (
                626
                185)
              object LanguageIdLabel: TLabel
                Left = 7
                Top = 57
                Width = 57
                Height = 13
                Caption = 'LanguageId'
              end
              object CharSetLabel: TLabel
                Left = 134
                Top = 57
                Width = 39
                Height = 13
                Caption = 'CharSet'
              end
              object _AppTitle: TLabeledEdit
                Left = 7
                Top = 30
                Width = 227
                Height = 21
                EditLabel.Width = 39
                EditLabel.Height = 13
                EditLabel.Caption = 'AppTitle'
                TabOrder = 0
              end
              object LanguageIdComboBox: TComboBox
                Left = 7
                Top = 72
                Width = 121
                Height = 21
                Style = csDropDownList
                DropDownCount = 20
                TabOrder = 2
              end
              object CharSetComboBox: TComboBox
                Left = 134
                Top = 72
                Width = 100
                Height = 21
                Style = csDropDownList
                DropDownCount = 20
                TabOrder = 3
              end
              object _HomeView: TLabeledEdit
                Left = 240
                Top = 30
                Width = 382
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                EditLabel.Width = 49
                EditLabel.Height = 13
                EditLabel.Caption = 'HomeView'
                TabOrder = 1
              end
              object _FOPEnginePath: TLabeledEdit
                Left = 7
                Top = 119
                Width = 615
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                EditLabel.Width = 74
                EditLabel.Height = 13
                EditLabel.Caption = 'FOPEnginePath'
                TabOrder = 5
              end
              object _JavascriptLibraries: TLabeledEdit
                Left = 7
                Top = 158
                Width = 615
                Height = 21
                Anchors = [akLeft, akTop, akRight]
                EditLabel.Width = 89
                EditLabel.Height = 13
                EditLabel.Caption = 'JavascriptLibraries'
                TabOrder = 6
              end
              object _LanguagePerSession: TCheckBox
                Left = 240
                Top = 74
                Width = 142
                Height = 17
                Caption = 'LanguagePerSession'
                TabOrder = 4
              end
            end
            object UserFormatsGroupBox: TGroupBox
              Left = 0
              Top = 185
              Width = 626
              Height = 57
              Align = alTop
              Caption = 'UserFormats'
              TabOrder = 2
              object _UserFormats_Time: TLabeledEdit
                Left = 132
                Top = 31
                Width = 100
                Height = 21
                EditLabel.Width = 22
                EditLabel.Height = 13
                EditLabel.Caption = 'Time'
                TabOrder = 1
              end
              object _UserFormats_Date: TLabeledEdit
                Left = 5
                Top = 31
                Width = 121
                Height = 21
                EditLabel.Width = 23
                EditLabel.Height = 13
                EditLabel.Caption = 'Date'
                TabOrder = 0
              end
            end
            object FastCGIGroupBox: TGroupBox
              Left = 0
              Top = 242
              Width = 626
              Height = 60
              Align = alTop
              Caption = 'FastCGI'
              TabOrder = 3
              object SessionTimeoutLabel: TLabel
                Left = 90
                Top = 18
                Width = 122
                Height = 13
                Caption = 'SessionTimeout (minutes)'
              end
              object TCPPortLabel: TLabel
                Left = 5
                Top = 18
                Width = 39
                Height = 13
                Caption = 'TCPPort'
              end
              object _FastCGI_TCPPort: TSpinEdit
                Left = 5
                Top = 33
                Width = 79
                Height = 22
                MaxValue = 0
                MinValue = 0
                TabOrder = 0
                Value = 0
              end
              object _FastCGI_SessionTimeout: TSpinEdit
                Left = 90
                Top = 33
                Width = 73
                Height = 22
                MaxValue = 0
                MinValue = 0
                TabOrder = 1
                Value = 0
              end
            end
            object ExtGroupBox: TGroupBox
              Left = 0
              Top = 302
              Width = 626
              Height = 60
              Align = alTop
              Caption = 'Ext'
              TabOrder = 4
              object ThemeLabel: TLabel
                Left = 5
                Top = 15
                Width = 32
                Height = 13
                Caption = 'Theme'
              end
              object AjaxTimeoutLabel: TLabel
                Left = 193
                Top = 15
                Width = 126
                Height = 13
                Caption = 'AjaxTimeout (milliseconds)'
              end
              object _Ext_URL: TLabeledEdit
                Left = 132
                Top = 30
                Width = 53
                Height = 21
                EditLabel.Width = 19
                EditLabel.Height = 13
                EditLabel.Caption = 'URL'
                TabOrder = 1
              end
              object _Ext_Theme: TComboBox
                Left = 5
                Top = 30
                Width = 121
                Height = 21
                Style = csDropDownList
                DropDownCount = 20
                TabOrder = 0
                Items.Strings = (
                  'default'
                  'access'
                  'blue'
                  'custom'
                  'freshmint'
                  'gray'
                  'peppermint')
              end
              object _Ext_AjaxTimeout: TSpinEdit
                Left = 192
                Top = 30
                Width = 73
                Height = 22
                Increment = 1000
                MaxValue = 0
                MinValue = 0
                TabOrder = 2
                Value = 0
              end
            end
          end
        end
        object DatabasesTabSheet: TTabSheet
          HelpType = htKeyword
          HelpKeyword = 'Config_Databases'
          Caption = 'Databases'
          ImageIndex = 2
          object TopPanel: TPanel
            Left = 0
            Top = 0
            Width = 626
            Height = 84
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
            DesignSize = (
              626
              84)
            object _DatabaseRouter: TLabeledEdit
              Left = 5
              Top = 16
              Width = 617
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              EditLabel.Width = 79
              EditLabel.Height = 13
              EditLabel.Caption = 'DatabaseRouter'
              TabOrder = 0
            end
            object _DefaultDatabaseName: TLabeledEdit
              Left = 5
              Top = 56
              Width = 617
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              EditLabel.Width = 108
              EditLabel.Height = 13
              EditLabel.Caption = 'DefaultDatabaseName'
              TabOrder = 1
            end
          end
          object DatabasesPanel: TPanel
            Left = 0
            Top = 84
            Width = 626
            Height = 353
            Align = alClient
            BevelOuter = bvLowered
            Padding.Top = 20
            TabOrder = 1
            object DatabasesLabel: TLabel
              Left = 5
              Top = 2
              Width = 51
              Height = 13
              Caption = 'Databases'
            end
          end
        end
        object AuthTabSheet: TTabSheet
          HelpType = htKeyword
          HelpKeyword = 'Config_Auth'
          Caption = 'Auth'
          ImageIndex = 3
        end
        object LoginTabSheet: TTabSheet
          Caption = 'Login'
          ImageIndex = 6
        end
        object AccessControlTabSheet: TTabSheet
          HelpType = htKeyword
          HelpKeyword = 'Config_AccessControl'
          Caption = 'AccessControl'
          ImageIndex = 5
        end
        object LogTabSheet: TTabSheet
          HelpType = htKeyword
          HelpKeyword = 'Config_Logging'
          Caption = 'Log'
          ImageIndex = 4
          object TextFileGroupBox: TGroupBox
            Left = 0
            Top = 44
            Width = 626
            Height = 58
            Align = alTop
            Caption = 'TextFile'
            TabOrder = 1
            DesignSize = (
              626
              58)
            object _Log_TextFile_FileName: TLabeledEdit
              Left = 5
              Top = 32
              Width = 593
              Height = 21
              Anchors = [akLeft, akTop, akRight]
              EditLabel.Width = 43
              EditLabel.Height = 13
              EditLabel.Caption = 'FileName'
              TabOrder = 1
            end
            object _Log_TextFile_IsEnabled: TCheckBox
              Left = 58
              Top = -1
              Width = 69
              Height = 17
              Caption = 'IsEnabled'
              TabOrder = 0
            end
          end
          object LogPanel: TPanel
            Left = 0
            Top = 0
            Width = 626
            Height = 44
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
            object LevelLabel: TLabel
              Left = 5
              Top = 1
              Width = 25
              Height = 13
              Caption = 'Level'
            end
            object _Log_Level: TSpinEdit
              Left = 5
              Top = 17
              Width = 42
              Height = 22
              MaxValue = 5
              MinValue = 0
              TabOrder = 0
              Value = 0
            end
          end
        end
        object EmailTabSheet: TTabSheet
          Caption = 'Email'
          ImageIndex = 5
        end
        object DefaultsTabSheet: TTabSheet
          Caption = 'Defaults'
          ImageIndex = 7
        end
      end
    end
    inherited PathPanel: TStaticText
      Width = 634
      ExplicitWidth = 634
    end
    inherited CodeEditorFrame: TCodeEditorFrame
      Top = 508
      Width = 634
      ExplicitTop = 508
      ExplicitWidth = 634
      inherited ErrorLabel: TLabel
        Width = 634
        ExplicitTop = 532
        ExplicitWidth = 590
      end
      inherited ToolBar: TToolBar
        Width = 634
        ExplicitWidth = 634
      end
    end
  end
  inherited TabSet: TTabSet
    Left = 634
    Height = 548
    ExplicitLeft = 634
    ExplicitHeight = 548
  end
end
