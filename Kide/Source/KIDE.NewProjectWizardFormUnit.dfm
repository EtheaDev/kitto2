inherited NewProjectWizardForm: TNewProjectWizardForm
  HelpContext = 120
  BorderStyle = bsDialog
  Caption = 'New Project'
  ClientHeight = 425
  ClientWidth = 562
  OnCreate = FormCreate
  ExplicitWidth = 568
  ExplicitHeight = 454
  PixelsPerInch = 96
  TextHeight = 13
  inherited PageControl: TPageControl
    Width = 562
    Height = 364
    ActivePage = OptionsTabSheet
    ExplicitWidth = 562
    ExplicitHeight = 364
    object SelectTabSheet: TTabSheet
      Caption = 'SelectTabSheet'
      object TemplateSplitter: TSplitter
        Left = 270
        Top = 0
        Height = 333
        Align = alRight
        AutoSnap = False
        MinSize = 50
        ExplicitLeft = 392
        ExplicitTop = 56
        ExplicitHeight = 100
      end
      inline TemplateFrame: TProjectTemplateFrame
        Left = 0
        Top = 0
        Width = 270
        Height = 333
        Align = alClient
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        ExplicitWidth = 270
        ExplicitHeight = 333
        inherited ListView: TListView
          Width = 270
          Height = 333
          ExplicitWidth = 270
          ExplicitHeight = 333
        end
      end
      object TemplateInfoPanel: TPanel
        Left = 273
        Top = 0
        Width = 281
        Height = 333
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 1
        object TemplateInfoRichEdit: TRichEdit
          Left = 0
          Top = 0
          Width = 281
          Height = 333
          Align = alClient
          Color = clBtnFace
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 0
          Zoom = 100
        end
      end
    end
    object OptionsTabSheet: TTabSheet
      Caption = 'OptionsTabSheet'
      ImageIndex = 1
      object AuthenticationtypeLabel: TLabel
        Left = 8
        Top = 137
        Width = 113
        Height = 13
        Caption = 'Authentication type'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object AccessControltypeLabel: TLabel
        Left = 8
        Top = 179
        Width = 112
        Height = 13
        Caption = 'Access Control type'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object ExtJSLabel: TLabel
        Left = 181
        Top = 138
        Width = 51
        Height = 13
        Caption = 'ExtJS 6.2'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object ThemeLabel: TLabel
        Left = 200
        Top = 157
        Width = 32
        Height = 13
        Alignment = taRightJustify
        Caption = 'Theme'
      end
      object LanguagEEncodingLabel: TLabel
        Left = 8
        Top = 221
        Width = 120
        Height = 13
        Caption = 'Language && Encoding'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LanguageLabel: TLabel
        Left = 3
        Top = 248
        Width = 47
        Height = 13
        Alignment = taRightJustify
        Caption = 'Language'
      end
      object CharsetLabel: TLabel
        Left = 12
        Top = 271
        Width = 38
        Height = 13
        Alignment = taRightJustify
        Caption = 'Charset'
      end
      object PortLabel: TLabel
        Left = 212
        Top = 241
        Width = 20
        Height = 13
        Alignment = taRightJustify
        Caption = 'Port'
      end
      object ServerLabel: TLabel
        Left = 189
        Top = 222
        Width = 41
        Height = 13
        Caption = 'Server:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object ThreadPoolSizeLabel: TLabel
        Left = 159
        Top = 265
        Width = 73
        Height = 13
        Alignment = taRightJustify
        Caption = 'ThreadPoolSize'
      end
      object SessionTimeOutLabel: TLabel
        Left = 156
        Top = 289
        Width = 76
        Height = 13
        Alignment = taRightJustify
        Caption = 'SessionTimeOut'
      end
      object AuthComboBox: TComboBox
        Left = 18
        Top = 154
        Width = 116
        Height = 21
        TabOrder = 2
      end
      object ACComboBox: TComboBox
        Left = 18
        Top = 195
        Width = 116
        Height = 21
        TabOrder = 3
      end
      object ExtThemeComboBox: TComboBox
        Left = 236
        Top = 154
        Width = 79
        Height = 21
        TabOrder = 4
        Text = 'triton'
        Items.Strings = (
          'classic')
      end
      object LanguageIdComboBox: TComboBox
        Left = 58
        Top = 238
        Width = 82
        Height = 21
        ItemIndex = 0
        TabOrder = 5
        Text = 'en'
        Items.Strings = (
          'en'
          'it')
      end
      object CharsetComboBox: TComboBox
        Left = 58
        Top = 261
        Width = 82
        Height = 21
        ItemIndex = 0
        TabOrder = 6
        Text = 'utf-8'
        Items.Strings = (
          'utf-8'
          'iso-8859-1')
      end
      object ServerPortEdit: TSpinEdit
        Left = 236
        Top = 238
        Width = 79
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 7
        Value = 8080
      end
      object DatabasesGroupBox: TGroupBox
        Left = 343
        Top = -1
        Width = 130
        Height = 89
        Caption = 'Database Adapters'
        TabOrder = 1
        object DBADOCheckBox: TCheckBox
          Left = 7
          Top = 17
          Width = 97
          Height = 17
          Caption = 'ADO'
          TabOrder = 0
        end
        object DBDBXCheckBox: TCheckBox
          Left = 7
          Top = 38
          Width = 97
          Height = 17
          Caption = 'DBExpress'
          TabOrder = 1
        end
        object DBFDCheckBox: TCheckBox
          Left = 7
          Top = 59
          Width = 97
          Height = 17
          Caption = 'FireDac'
          TabOrder = 2
        end
      end
      object DelphiGroupBox: TGroupBox
        Left = 3
        Top = 1
        Width = 326
        Height = 87
        Caption = 'Create Delphi Project'
        TabOrder = 0
        object SearchPathLabel: TLabel
          Left = 136
          Top = 18
          Width = 83
          Height = 13
          Caption = 'Kitto Search Path'
        end
        object SearchPathComboBox: TComboBox
          Left = 136
          Top = 33
          Width = 177
          Height = 21
          Hint = 'Root Kitto directory as seen by the Delphi project'
          TabOrder = 2
        end
        object D10_2CheckBox: TCheckBox
          Left = 8
          Top = 22
          Width = 110
          Height = 17
          Caption = 'Delphi 10.2 Tokyo'
          TabOrder = 0
        end
        object D10_3CheckBox: TCheckBox
          Left = 8
          Top = 40
          Width = 110
          Height = 17
          Caption = 'Delphi 10.3 Rio'
          TabOrder = 1
        end
        object D10_4CheckBox: TCheckBox
          Left = 8
          Top = 58
          Width = 110
          Height = 17
          Caption = 'Delphi 10.4 Sydney'
          TabOrder = 3
        end
      end
      object ServerThreadPoolSizeEdit: TSpinEdit
        Left = 236
        Top = 262
        Width = 79
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 8
        Value = 20
      end
      object ServerSessionTimeOutEdit: TSpinEdit
        Left = 236
        Top = 286
        Width = 79
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 9
        Value = 10
      end
    end
    object GoTabSheet: TTabSheet
      Caption = 'GoTabSheet'
      ImageIndex = 2
      DesignSize = (
        554
        333)
      object ProjectPathButton: TSpeedButton
        Left = 517
        Top = 32
        Width = 23
        Height = 22
        Hint = 'Select an empty directory for the new project'
        Anchors = [akTop, akRight]
        Caption = '...'
        OnClick = ProjectPathButtonClick
        ExplicitLeft = 653
      end
      object ProjectPathEdit: TLabeledEdit
        Left = 24
        Top = 32
        Width = 487
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 105
        EditLabel.Height = 13
        EditLabel.Caption = 'New Project Directory'
        TabOrder = 0
      end
      object ProjectNameEdit: TLabeledEdit
        Left = 24
        Top = 80
        Width = 137
        Height = 21
        EditLabel.Width = 64
        EditLabel.Height = 13
        EditLabel.Caption = 'Project Name'
        TabOrder = 1
        OnChange = ProjectNameEditChange
      end
      object AppTitleEdit: TLabeledEdit
        Left = 167
        Top = 80
        Width = 231
        Height = 21
        EditLabel.Width = 75
        EditLabel.Height = 13
        EditLabel.Caption = 'Application Title'
        TabOrder = 2
      end
    end
    object DoneTabSheet: TTabSheet
      Caption = 'DoneTabSheet'
      ImageIndex = 3
      object ProjectCreatedRichEdit: TRichEdit
        Left = 0
        Top = 0
        Width = 554
        Height = 333
        Align = alClient
        Color = clBtnFace
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
        Zoom = 100
      end
    end
  end
  inherited ButtonPanel: TPanel
    Top = 389
    Width = 562
    ExplicitTop = 389
    ExplicitWidth = 562
    inherited BackButton: TButton
      Left = 402
      ExplicitLeft = 402
    end
    inherited ForwardButton: TButton
      Left = 483
      ExplicitLeft = 483
    end
  end
  inherited TitlePanel: TPanel
    Width = 552
    ExplicitWidth = 552
  end
  inherited ActionList: TActionList
    Left = 496
  end
end
