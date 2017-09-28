inherited MainForm: TMainForm
  HelpKeyword = 'KittoEntKIDE'
  HelpContext = 1140
  Caption = 'Kide'
  ClientHeight = 459
  ClientWidth = 684
  OnCreate = FormCreate
  ExplicitWidth = 700
  ExplicitHeight = 498
  PixelsPerInch = 96
  TextHeight = 13
  object MainMenuBar: TActionMainMenuBar
    Left = 0
    Top = 0
    Width = 684
    Height = 25
    UseSystemFont = False
    ActionManager = ActionManager
    Caption = 'MainMenuBar'
    Color = clMenuBar
    ColorMap.DisabledFontColor = 7171437
    ColorMap.HighlightColor = clWhite
    ColorMap.BtnSelectedFont = clBlack
    ColorMap.UnusedColor = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = []
    Spacing = 0
  end
  object MainActionToolBar: TActionToolBar
    Left = 0
    Top = 25
    Width = 684
    Height = 26
    ActionManager = ActionManager
    Caption = 'Main'
    Color = clMenuBar
    ColorMap.DisabledFontColor = 7171437
    ColorMap.HighlightColor = clWhite
    ColorMap.BtnSelectedFont = clBlack
    ColorMap.UnusedColor = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Spacing = 0
  end
  object ClientPanel: TPanel
    Left = 0
    Top = 51
    Width = 684
    Height = 389
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object BottomSplitter: TSplitter
      Left = 0
      Top = 284
      Width = 684
      Height = 4
      Cursor = crVSplit
      Align = alBottom
      AutoSnap = False
      ResizeStyle = rsUpdate
      OnMoved = BottomSplitterMoved
      ExplicitTop = 285
    end
    object BrowseSplitter: TSplitter
      Left = 233
      Top = 0
      Width = 4
      Height = 284
      AutoSnap = False
      MinSize = 50
      ResizeStyle = rsUpdate
      OnMoved = BrowseSplitterMoved
      ExplicitHeight = 285
    end
    object BrowsePanel: TPanel
      Left = 0
      Top = 0
      Width = 233
      Height = 284
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object BrowsePageControl: TPageControl
        Left = 0
        Top = 0
        Width = 233
        Height = 284
        HelpContext = 2000
        ActivePage = MetadataTabSheet
        Align = alClient
        Images = MainDataModule.Images
        MultiLine = True
        TabOrder = 0
        object MetadataTabSheet: TTabSheet
          Hint = 'Files of metadata'
          Caption = 'Metadata'
          ImageIndex = 80
          object MetadataTreeView: TTreeView
            Left = 0
            Top = 26
            Width = 225
            Height = 229
            Align = alClient
            Images = MainDataModule.Images
            Indent = 19
            MultiSelect = True
            MultiSelectStyle = [msControlSelect, msShiftSelect]
            PopupMenu = TreePopupMenu
            ReadOnly = True
            TabOrder = 0
            OnCreateNodeClass = BrowseTreeViewCreateNodeClass
            OnDblClick = BrowseTreeViewDblClick
            OnMouseDown = BrowseTreeViewMouseDown
          end
          object MetadataActionToolBar: TActionToolBar
            Left = 0
            Top = 0
            Width = 225
            Height = 26
            ActionManager = ActionManager
            Caption = 'Metadata'
            Color = clMenuBar
            ColorMap.DisabledFontColor = 7171437
            ColorMap.HighlightColor = clWhite
            ColorMap.BtnSelectedFont = clBlack
            ColorMap.UnusedColor = clWhite
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            Spacing = 0
          end
        end
        object TTabSheet
          Hint = 'Files of resources'
          Caption = 'Resources'
          ImageIndex = 20
          object ResourcesActionToolBar: TActionToolBar
            Left = 0
            Top = 0
            Width = 225
            Height = 26
            ActionManager = ActionManager
            Caption = 'Resources'
            Color = clMenuBar
            ColorMap.DisabledFontColor = 7171437
            ColorMap.HighlightColor = clWhite
            ColorMap.BtnSelectedFont = clBlack
            ColorMap.UnusedColor = clWhite
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            Spacing = 0
          end
          object ResourcesTreeView: TTreeView
            Left = 0
            Top = 26
            Width = 225
            Height = 229
            Align = alClient
            Images = MainDataModule.Images
            Indent = 19
            MultiSelect = True
            MultiSelectStyle = [msControlSelect, msShiftSelect]
            PopupMenu = TreePopupMenu
            ReadOnly = True
            TabOrder = 1
            OnCreateNodeClass = BrowseTreeViewCreateNodeClass
            OnDblClick = BrowseTreeViewDblClick
            OnMouseDown = BrowseTreeViewMouseDown
          end
        end
      end
    end
    object EditPanel: TPanel
      Left = 237
      Top = 0
      Width = 447
      Height = 284
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object ClientImage: TImage
        Left = 0
        Top = 0
        Width = 447
        Height = 284
        Align = alClient
        Center = True
        Stretch = True
        ExplicitLeft = 288
        ExplicitTop = 144
        ExplicitWidth = 105
        ExplicitHeight = 105
      end
      object EditPageControl: TPageControl
        Left = 0
        Top = 0
        Width = 447
        Height = 284
        Align = alClient
        Images = MainDataModule.Images
        TabOrder = 0
        OnContextPopup = EditPageControlContextPopup
        OnMouseDown = EditPageControlMouseDown
        OnMouseLeave = EditPageControlMouseLeave
        OnMouseMove = EditPageControlMouseMove
      end
    end
    object BottomPanel: TPanel
      Left = 0
      Top = 288
      Width = 684
      Height = 101
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      object BottomPageControl: TPageControl
        Left = 0
        Top = 0
        Width = 684
        Height = 101
        ActivePage = LogTabSheet
        Align = alClient
        Images = MainDataModule.Images
        MultiLine = True
        TabOrder = 0
        TabPosition = tpLeft
        object LogTabSheet: TTabSheet
          Hint = 'Messages'
          Caption = 'Messages'
          ImageIndex = 86
          object LogListBox: TListBox
            Left = 0
            Top = 0
            Width = 656
            Height = 93
            HelpContext = 180
            Style = lbOwnerDrawFixed
            Align = alClient
            ItemHeight = 13
            MultiSelect = True
            PopupMenu = MessagesPopupActionBar
            TabOrder = 0
            OnDrawItem = LogListBoxDrawItem
          end
        end
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 440
    Width = 684
    Height = 19
    Panels = <
      item
        Width = 50
      end
      item
        Width = 50
      end>
  end
  object ActionManager: TActionManager
    ActionBars = <
      item
        Items.CaptionOptions = coNone
        Items = <
          item
            Action = NewProjectAction
            Caption = '&New Project...'
            ImageIndex = 13
          end
          item
            Action = OpenProjectAction
            Caption = '&Open Project...'
            ImageIndex = 1
          end
          item
            Action = CloseProjectAction
            Caption = '&Close Project'
            ImageIndex = 9
          end
          item
            Action = ExitAction
            Caption = '&Exit'
            ImageIndex = 2
          end
          item
            Action = SaveAllAction
            Caption = 'S&ave All'
            ImageIndex = 24
          end
          item
            Action = ChangeStyleAction
            Caption = 'C&hange style...'
            ImageIndex = 57
          end
          item
            Caption = '-'
          end
          item
            Action = ReloadAction
            Caption = '&Undo/Reload'
            ImageIndex = 66
          end
          item
            Action = SaveAction
            Caption = '&Save'
            ImageIndex = 23
          end
          item
            Caption = '-'
          end
          item
            Action = StartAction
            Caption = 'S&tart'
            ImageIndex = 55
          end
          item
            Caption = '-'
          end
          item
            Action = HelpKideAction
            Caption = 'K&ide Help...'
            ImageIndex = 76
          end
          item
            Action = HelpKittoAction
            Caption = 'On&line Kitto Wiki...'
            ImageIndex = 77
          end
          item
            Action = KittoRefAction
            Caption = '&Kitto2 library reference...'
            ImageIndex = 21
          end>
        ActionBar = MainActionToolBar
      end
      item
        Items = <
          item
            Items = <
              item
                Action = NewProjectAction
                Caption = '&New Project...'
                ImageIndex = 13
              end
              item
                Items = <
                  item
                    Action = OpenProjectAction
                    Caption = '&Open Project...'
                    ImageIndex = 1
                  end
                  item
                    Caption = '-'
                  end>
                Caption = '&Open Project'
              end
              item
                Action = CloseProjectAction
                Caption = '&Close Project'
                ImageIndex = 9
              end
              item
                Action = SaveAllAction
                Caption = 'S&ave All'
                ImageIndex = 24
              end
              item
                Caption = '-'
              end
              item
                Action = ReloadAction
                Caption = '&Undo/Reload'
                ImageIndex = 66
              end
              item
                Action = SaveAction
                Caption = '&Save'
                ImageIndex = 23
              end
              item
                Action = CloseAction
                Caption = 'C&lose'
                ImageIndex = 26
              end
              item
                Caption = '-'
              end
              item
                Action = ExitAction
                Caption = '&Exit'
                ImageIndex = 2
              end>
            Caption = '&File'
          end
          item
            Items = <
              item
                Action = ViewProjectFilesAction
                Caption = '&Project files panel'
                ImageIndex = 72
                ShowGlyph = False
              end
              item
                Action = ViewLogAction
                Caption = '&Messages panel'
                ImageIndex = 34
                ShowGlyph = False
              end
              item
                Caption = '-'
              end
              item
                Action = ChangeStyleAction
                Caption = '&Change style...'
                ImageIndex = 57
              end>
            Caption = '&View'
          end
          item
            Items = <
              item
                Action = RefreshMetadataAction
                Caption = '&Refresh'
                ImageIndex = 6
              end
              item
                Action = ModelWizardAction
                Caption = '&Model Wizard...'
                ImageIndex = 7
              end
              item
                Action = ValidateMetadataAction
                Caption = '&Validate Metadata'
                ImageIndex = 15
              end>
            Caption = '&Metadata'
          end
          item
            Items = <
              item
                Action = ResourcesRefreshAction
                Caption = '&Refresh'
                ImageIndex = 6
              end
              item
                Action = UpdateLocaleFilesAction
                Caption = '&Update Locale Files...'
                ImageIndex = 10
              end>
            Visible = False
            Caption = '&Resources'
          end
          item
            Items = <
              item
                Action = StartAction
                Caption = '&Start Kitto engine'
                ImageIndex = 55
              end>
            Caption = '&Run'
          end
          item
            Items = <
              item
                Action = HelpKideAction
                Caption = '&Kide Help...'
                ImageIndex = 76
              end
              item
                Action = HelpKittoAction
                Caption = '&Online Kitto Wiki...'
                ImageIndex = 77
              end
              item
                Action = KittoRefAction
                Caption = 'K&itto library reference...'
                ImageIndex = 21
              end
              item
                Caption = '-'
              end
              item
                Action = AboutAction
                Caption = '&About...'
                ImageIndex = 68
              end>
            Caption = '&Help'
          end>
        ActionBar = MainMenuBar
      end
      item
        Items.CaptionOptions = coNone
        Items = <
          item
            Action = RefreshMetadataAction
            Caption = '&Refresh'
            ImageIndex = 6
          end
          item
            Action = ModelWizardAction
            Caption = '&Model Wizard...'
            ImageIndex = 7
          end
          item
            Action = ValidateMetadataAction
            Caption = '&Validate Metadata'
            ImageIndex = 15
          end>
        ActionBar = MetadataActionToolBar
      end
      item
        Items.CaptionOptions = coNone
        Items = <
          item
            Action = ResourcesRefreshAction
            Caption = '&Refresh'
            ImageIndex = 6
          end
          item
            Action = UpdateLocaleFilesAction
            Caption = '&Update Locale Files...'
            ImageIndex = 10
          end>
        ActionBar = ResourcesActionToolBar
      end>
    Images = MainDataModule.Images
    Left = 368
    Top = 144
    StyleName = 'Platform Default'
    object NewProjectAction: TAction
      Category = 'File'
      Caption = 'New Project...'
      Hint = 'Create a new Kide project from a template'
      ImageIndex = 13
      OnExecute = NewProjectActionExecute
    end
    object ViewProjectFilesAction: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Project files panel'
      Checked = True
      Hint = 'Show or hide the Project Files panel'
      ImageIndex = 72
      OnExecute = ViewProjectFilesActionExecute
    end
    object ChangeIconsAction: TAction
      Category = 'View'
      Caption = 'Change icons...'
      ImageIndex = 57
      OnExecute = ChangeStyleActionExecute
      OnUpdate = ChangeStyleActionUpdate
    end
    object OpenProjectAction: TAction
      Category = 'File'
      Caption = 'Open Project...'
      Hint = 'Open an existing Kide project'
      ImageIndex = 1
      OnExecute = OpenProjectActionExecute
    end
    object CloseProjectAction: TAction
      Category = 'File'
      Caption = 'Close Project'
      Hint = 'Close the currently open project'
      ImageIndex = 9
      OnExecute = CloseProjectActionExecute
      OnUpdate = CloseProjectActionUpdate
    end
    object ExitAction: TAction
      Category = 'File'
      Caption = 'Exit'
      Hint = 'Exit Kide'
      ImageIndex = 2
      OnExecute = ExitActionExecute
    end
    object RefreshMetadataAction: TAction
      Category = 'Metadata'
      Caption = 'Refresh'
      Hint = 'Refresh metadata from files'
      ImageIndex = 6
      OnExecute = RefreshMetadataActionExecute
      OnUpdate = RefreshMetadataActionUpdate
    end
    object ModelWizardAction: TAction
      Category = 'Metadata'
      Caption = 'Model Wizard...'
      Hint = 'Create or update models from a live database'
      ImageIndex = 7
      OnExecute = ModelWizardActionExecute
      OnUpdate = ModelWizardActionUpdate
    end
    object UpdateLocaleFilesAction: TAction
      Category = 'Resources'
      Caption = 'Update Locale Files...'
      Hint = 'Update all locale files'
      ImageIndex = 10
      OnExecute = UpdateLocaleFilesActionExecute
      OnUpdate = UpdateLocaleFilesActionUpdate
    end
    object AboutAction: TAction
      Category = 'Help'
      Caption = 'About...'
      Hint = 'Application information'
      ImageIndex = 68
      OnExecute = AboutActionExecute
    end
    object ViewLogAction: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Messages panel'
      Checked = True
      Hint = 'Show or hide the Messages panel'
      ImageIndex = 34
      OnExecute = ViewLogActionExecute
    end
    object ValidateMetadataAction: TAction
      Category = 'Metadata'
      Caption = 'Validate Metadata'
      Hint = 'Detect and report errors in the metadata files'
      ImageIndex = 15
      OnExecute = ValidateMetadataActionExecute
      OnUpdate = ValidateMetadataActionUpdate
    end
    object ClearMessagesAction: TAction
      Category = 'Messages'
      Caption = 'Clear'
      ImageIndex = 17
      OnExecute = ClearMessagesActionExecute
    end
    object CopyMessagesAction: TAction
      Category = 'Messages'
      Caption = 'Copy'
      ImageIndex = 16
      OnExecute = CopyMessagesActionExecute
      OnUpdate = CopyMessagesActionUpdate
    end
    object ResourcesRefreshAction: TAction
      Category = 'Resources'
      Caption = 'Refresh'
      Hint = 'Re-read Resources directory tree'
      ImageIndex = 6
      OnExecute = ResourcesRefreshActionExecute
      OnUpdate = ResourcesRefreshActionUpdate
    end
    object SaveAction: TAction
      Category = 'Editor'
      Caption = 'Save'
      Hint = 'Save changes to the file currently open in the editor'
      ImageIndex = 23
      OnExecute = SaveActionExecute
      OnUpdate = SaveActionUpdate
    end
    object SaveAllAction: TAction
      Category = 'Editor'
      Caption = 'Save All'
      Hint = 'Save all pending changes in all open files'
      ImageIndex = 24
      OnExecute = SaveAllActionExecute
      OnUpdate = SaveAllActionUpdate
    end
    object CloseAction: TAction
      Category = 'Editor'
      Caption = 'Close'
      ImageIndex = 26
      OnExecute = CloseActionExecute
      OnUpdate = CloseActionUpdate
    end
    object StartAction: TAction
      Category = 'Run'
      Caption = 'Start Kitto engine'
      Hint = 'Start Kitto engine of current project...'
      ImageIndex = 55
      OnExecute = StartActionExecute
      OnUpdate = StartActionUpdate
    end
    object ChangeStyleAction: TAction
      Category = 'View'
      Caption = 'Change style...'
      ImageIndex = 57
      OnExecute = ChangeStyleActionExecute
      OnUpdate = ChangeStyleActionUpdate
    end
    object ReloadAction: TAction
      Category = 'Editor'
      Caption = 'Undo/Reload'
      Hint = 
        'Undo changes of the file currently open in the editor (reload fr' +
        'om disk)'
      ImageIndex = 66
      OnExecute = ReloadActionExecute
      OnUpdate = ReloadActionUpdate
    end
    object HelpKittoAction: TAction
      Category = 'Help'
      Caption = 'Online Kitto Wiki...'
      Hint = 'Online Kitto Wiki documentation'
      ImageIndex = 77
      OnExecute = HelpKittoActionExecute
    end
    object HelpKideAction: TAction
      Category = 'Help'
      Caption = 'Kide Help...'
      Hint = 'Online Kide Help Guide'
      ImageIndex = 76
      OnExecute = HelpKideActionExecute
    end
    object KittoRefAction: TAction
      Category = 'Help'
      Caption = 'Kitto library reference...'
      Hint = 'Online Kitto library reference'
      ImageIndex = 21
      OnExecute = KittoRefActionExecute
    end
  end
  object OpenProjectDialog: TOpenDialog
    DefaultExt = '.kproj'
    Filter = 'Kide Projects (*.kproj)|*.kproj|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Open Kide Project'
    Left = 144
    Top = 64
  end
  object TreePopupMenu: TPopupMenu
    Images = MainDataModule.Images
    MenuAnimation = [maLeftToRight, maTopToBottom]
    OnPopup = TreePopupMenuPopup
    Left = 128
    Top = 136
  end
  object ApplicationEvents: TApplicationEvents
    OnHint = ApplicationEventsHint
    Left = 256
    Top = 176
  end
  object NewProjectDialog: TSaveDialog
    DefaultExt = '.kproj'
    Filter = 'Kide Projects (*.kproj)|*.kproj|All Files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Title = 'New Kide Project'
    Left = 216
    Top = 48
  end
  object LogImages: TImageList
    ColorDepth = cd32Bit
    DrawingStyle = dsTransparent
    Left = 448
    Top = 240
    Bitmap = {
      494C010103000500480010001000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000003300000033000000330000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000003300000033000000330000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003981ACFF3880ABFF3981ACFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000003300000033000000330000
      0033000000330000003300000033000000330000003300000033000000330000
      0033000000330000003300000033000000333981ACFF3880ABFF3981ACFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000377EABFF6AB2D4FF377EABFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003982CAFF387EC8FF377CC7FF377B
      C7FF387BC7FF387BC7FF377CC7FF377DC9FF377DC9FF377CC7FF387BC7FF387B
      C7FF377BC7FF377CC7FF387EC8FF3982CAFF377EABFF6AB2D4FF3880ABFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000377EAAFF68B0D3FF357EABFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003A86CDFF54E3FFFF54E3FFFF55E1
      FFFF55E1FFFF55E2FFFF55E5FFFF54E9FFFF54E9FFFF55E5FFFF55E2FFFF55E1
      FFFF55E1FFFF54E3FFFF54E3FFFF3A86CDFF377EAAFF68B0D3FF377EAAFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000377EAAFF66B0D4FF2F7DB0FF0000
      0000000000000000000000000000000000000000000F00000032000000330000
      003300000033000000210000000A00000000132A439479B8E4FF51DBFFFF4AD5
      FFFF4CD5FFFF4CD6FFFF4ADCFFFF66483EFF66483EFF4ADCFFFF4CD6FFFF4CD5
      FFFF4AD5FFFF51DBFFFF79B8E4FF132A4394377EAAFF68B1D2FF3782A9FF0000
      0000000000000000000000000000000000000000000F00000032000000330000
      003300000033000000210000000A000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000367EABFF62B0D7FF217CBBFF0000
      00200000000D0000000000000000000000001B11056DDA8C27FBE28F26FFE18C
      22FFE08B21FF6A4210B9100A025D0000001F000000003E82C9FFA3E0FAFF3FCF
      FFFF43CEFFFF45CFFFFF43D6FFFF765B4FFF765B4FFF43D6FFFF45CFFFFF43CE
      FFFF3FCFFFFFA3E0FAFF3E82C9FF00000000377EAAFF69B3D2FF3887A7FF0000
      00200000000D0000000000000000000000000507176D2E3DBDFB2D3DC2FF2B3A
      C0FF2A39BFFF151B5BB903040E5D0000001F0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000347EACFF65ADD0FFEC8E1BFF643E
      0EB1180F0469000000330000003300000033E29027FFE28F26FFE38E23FFFFDF
      8BFFFFD981FFFCBC53FFEF9E2FFF5B3A0FAD0000000003090E445A97D4FF9EE8
      FFFF37C7FFFF3CC8FFFF3CCFFFFF4ABCE9FF4ABCE9FF3CCFFFFF3CC8FFFF37C7
      FFFF9EE8FFFF5A97D4FF03090E44000000003780AAFF65ADD1FF2B36C3FF1319
      53B1040614690000003300000033000000332E3EC3FF2D3DC2FF2A3BC2FF708C
      FFFF7793FFFF4D67ECFF3046D4FF12194FAD0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000347DACFF57AEE0FFF18D14FFF8A2
      29FFE9992DFFE08B22FFE08C22FFE08B21FFE08B20FFEA972AFFDF881DFFFFDB
      85FFFFD37AFFFFC358FFFFCF7DFFE08C22FF00000000000000001B436CBB90BF
      E6FF6ED7FFFF31C3FFFF33CBFFFF634A3EFF634A3EFF33CBFFFF31C3FFFF6ED7
      FFFF90BFE6FF1B436CBB00000000000000003780AAFF69B6D0FF2932C2FF2941
      D8FF3143CCFF2B3ABFFF2A39BFFF2A39BFFF2939BFFF2F41CCFF2736BDFF6C88
      FFFF728EFEFF4F6BF4FF798DF5FF2A3AC0FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000347DACFF56AEE0FFEF8B12FFFFCB
      73FFFFBA47FFFFC760FFFECA6BFFF9CA71FFFFD071FFFFD688FFDE8519FFFFDA
      84FFFFD278FFFFC055FFFFD58EFFE08B20FF0000000000000000000000003B82
      C9FFC1E8FDFF4FCCFFFF26C5FFFF68544BFF68544BFF26C5FFFF4FCCFFFFC1E8
      FDFF3B82C9FF0000000000000000000000003780AAFF68B6CFFF2630C0FF7185
      F2FF3D5BEFFF5673F7FF647EF7FF6780F2FF5C77F8FF8394F9FF2432BBFF6B87
      FEFF718CFDFF4C68F2FF8B9BF5FF2837BFFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000347DACFF56ADDFFFEE8A10FFFFD2
      85FFFFB643FFFFC25BFFFFCF70FFFFDB89FFFFD171FFFFD995FFDD8316FFFFD9
      83FFFFD278FFFFC053FFFFDBA0FFE08A1EFF000000000000000000000000060F
      17585697D5FFC6F3FFFF39CAFFFF6B5A53FF6B5A53FF39CAFFFFC6F3FFFF5697
      D5FF060F17580000000000000000000000003780AAFF68B6CFFF242EBFFF8494
      F3FF3A57ECFF536EF3FF6985FBFF7995FFFF5B76F8FF90A0F7FF2130B9FF6A86
      FEFF708CFDFF4B67F2FF9DAAF7FF2636BFFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000347DACFF56ADDFFFEE890EFFFFD9
      98FFFFB540FFFFC159FFFFCD6EFFFFD987FFFFCF6EFFFFDEA5FFDD8313FFFFD9
      81FFFFD175FFFFBF51FFFFE2B2FFE08A1DFF0000000000000000000000000000
      0000215283CF86BEEAFFA4ECFFFF6B5C52FF6B5C52FFA4ECFFFF86BEEAFF2152
      83CF000000000000000000000000000000003780AAFF68B6CFFF222CBFFF97A3
      F5FF3855ECFF516DF2FF6783F9FF7792FEFF5873F6FFA1AEF9FF1E2EB9FF6784
      FEFF6E8AFDFF4865F2FFAEB9F9FF2535BFFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000347DACFF56ADDFFFEE880DFFFFDF
      AAFFFFB43EFFFFC159FFFFCD6EFFFFD986FFFFCE6CFFFFE5B5FFDD8210FFFFE7
      A9FFFFD06FFFFFBD4AFFFFE9C1FFE0891BFF0000000000000000000000000000
      00000000000A3983CBFFB2E5FFFF9EC8DEFF9EC8DEFFB2E5FFFF3983CBFF0000
      000A000000000000000000000000000000003780AAFF68B5CFFF212BBFFFA9B4
      F7FF3553EBFF516CF2FF6783F9FF7792FEFF5672F6FFB2BDFAFF1A2AB9FF97AB
      FFFF6784FEFF415FF2FFBEC7FAFF2333BFFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000347DACFF56ADDEFFEE880BFFFFE6
      BCFFFFB239FFFFBF54FFFFCC6AFFFFD883FFFFCD69FFFFEBC4FFDE8310FFFCEE
      DBFFFDF3E4FFFFF3E0FFFFF7E1FFE0891AFF0000000000000000000000000000
      0000000000000917246D5298D7FFC1F0FFFFC1F0FFFF5298D7FF0917246D0000
      0000000000000000000000000000000000003780AAFF67B5CFFF1F2ABFFFBAC3
      FAFF304EEBFF4B68F1FF6380F9FF738FFEFF536FF6FFC2CBFCFF192ABBFFDADE
      F7FFE4E8FBFFDEE3FCFFDEE5FFFF2232BFFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000347DACFF57AEDEFFF0890BFFFFFC
      ECFFFFDBA0FFFFD792FFFFD584FFFFDF97FFFFD683FFFFF8E5FFE08615FFE28F
      25FFE6A24AFFE8A754FFF9E5CAFFE18B1EFF0000000000000000000000000000
      00000000000000000000215283CF84C3F1FF84C3F1FF215283CF000000000000
      0000000000000000000000000000000000003780AAFF68B5D0FF202ABFFFEBEE
      FFFF9AAAF7FF8DA0F7FF7C93FBFF8AA1FFFF7088F9FFE3E9FFFF1E2EBDFF2D3D
      C3FF505DCCFF5966CFFFCBD0F1FF2636C1FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000357EADFF5BB0DFFFF68C0DFFF5AD
      4FFFF8C986FFFFE4B9FFFFF9E1FFFFFDEAFFFFFAE2FFFDDCA9FFE18B1DFF0000
      0000080501311A1003577B4B11BCE28F24FF0000000000000000000000000000
      00000000000000000000000203213284CDFF3284CDFF00020321000000000000
      0000000000000000000000000000000000003880ABFF69B7D2FF252EC3FF5564
      D9FF8492E9FFB7C2F8FFDEE6FFFFE4EBFFFFDAE2FFFFA8B3F4FF2435C0FF0000
      00000102073104061657151E69BC2C3CC3FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001B3E52B12E7EB3FF273639A42516
      0466925913CDE0891BFFE08819FFE08818FFE0881AFFD5831DF840280A880000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000001B3E52B13884AAFF162E49A40608
      1F6618237CCD2334BFFF2132BEFF2131BEFF2232BFFF2433B6F80C1136880000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000}
  end
  object MessagesPopupActionBar: TPopupActionBar
    Images = MainDataModule.Images
    MenuAnimation = [maLeftToRight, maTopToBottom]
    Left = 296
    Top = 360
    object Clear1: TMenuItem
      Action = ClearMessagesAction
    end
    object Copy1: TMenuItem
      Action = CopyMessagesAction
    end
    object Messages1: TMenuItem
      Action = ViewLogAction
      AutoCheck = True
      Caption = 'Hide Messages panel'
    end
  end
  object TabPopupMenu: TPopupActionBar
    Images = MainDataModule.Images
    MenuAnimation = [maLeftToRight, maTopToBottom]
    Left = 312
    Top = 240
    object Save1: TMenuItem
      Action = SaveAction
    end
    object UndoReload1: TMenuItem
      Action = ReloadAction
      Default = True
    end
    object Close1: TMenuItem
      Action = CloseAction
      ImageIndex = 33
    end
  end
  object EditorPopupMenu: TPopupActionBar
    MenuAnimation = [maLeftToRight, maTopToBottom]
    Left = 376
    Top = 272
  end
end
