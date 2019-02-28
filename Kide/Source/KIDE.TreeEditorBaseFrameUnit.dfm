inherited TreeEditorBaseFrame: TTreeEditorBaseFrame
  Width = 601
  Height = 454
  ExplicitWidth = 601
  ExplicitHeight = 454
  object ClientPanel: TPanel
    Left = 0
    Top = 26
    Width = 601
    Height = 428
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object BottomSplitter: TSplitter
      Left = 0
      Top = 325
      Width = 601
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      AutoSnap = False
      ResizeStyle = rsUpdate
      OnMoved = BottomSplitterMoved
      ExplicitTop = 352
    end
    object DesignPanel: TPanel
      Left = 0
      Top = 0
      Width = 601
      Height = 325
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitHeight = 321
      object DesignerSplitter: TSplitter
        Left = 285
        Top = 0
        Height = 325
        Align = alRight
        AutoSnap = False
        ResizeStyle = rsUpdate
        OnMoved = DesignerSplitterMoved
        ExplicitLeft = 256
        ExplicitTop = 120
        ExplicitHeight = 100
      end
      object DesignerPanel: TPanel
        Left = 288
        Top = 0
        Width = 313
        Height = 325
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitHeight = 321
      end
    end
    object EditPanel: TPanel
      Left = 0
      Top = 328
      Width = 601
      Height = 100
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      inline CodeEditorFrame: TCodeEditorFrame
        Left = 0
        Top = 0
        Width = 601
        Height = 100
        Align = alClient
        DoubleBuffered = False
        ParentDoubleBuffered = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        ExplicitWidth = 601
        ExplicitHeight = 100
        inherited ErrorLabel: TLabel
          Top = 84
          Width = 601
          ExplicitTop = 85
          ExplicitWidth = 601
        end
        inherited ToolBar: TToolBar
          Width = 601
          ExplicitWidth = 601
        end
      end
    end
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 601
    Height = 26
    AutoSize = True
    ButtonHeight = 26
    ButtonWidth = 26
    Images = MainDataModule.Images
    TabOrder = 1
    object ShowCodeButton: TToolButton
      Left = 0
      Top = 0
      Action = ShowCodeAction
      Style = tbsCheck
    end
    object ShowPreviewButton: TToolButton
      Left = 26
      Top = 0
      Action = ShowPreviewAction
    end
    object ToolButtonTreeSep1: TToolButton
      Left = 52
      Top = 0
      Width = 8
      ImageIndex = 57
      Style = tbsSeparator
    end
    object CollapseNodeToolButton: TToolButton
      Left = 60
      Top = 0
      Action = CollapseNodeAction
    end
    object ExpandNodeActionToolButton: TToolButton
      Left = 86
      Top = 0
      Action = ExpandNodeAction
    end
    object ToolButtonTreeSep2: TToolButton
      Left = 112
      Top = 0
      Width = 8
      ImageIndex = 30
      Style = tbsSeparator
    end
    object SearchToolButton: TToolButton
      Left = 120
      Top = 0
      Action = SearchAction
    end
    object SearchAgainToolButton: TToolButton
      Left = 146
      Top = 0
      Action = SearchAgainAction
    end
    object ToolButtonTreeSep3: TToolButton
      Left = 172
      Top = 0
      Width = 8
      Caption = 'ToolButtonTreeSep3'
      ImageIndex = 60
      Style = tbsSeparator
    end
    object ValidateToolButton: TToolButton
      Left = 180
      Top = 0
      Action = ValidateAction
    end
    object ToolButtonTreeSep4: TToolButton
      Left = 206
      Top = 0
      Width = 8
      ImageIndex = 16
      Style = tbsSeparator
    end
    object UndoToolButton: TToolButton
      Left = 214
      Top = 0
      Action = ReloadAction
    end
    object SaveToolButton: TToolButton
      Left = 240
      Top = 0
      Action = SaveAction
    end
  end
  object ActionList: TActionList
    Images = MainDataModule.Images
    OnUpdate = ActionListUpdate
    Left = 344
    Top = 152
    object ShowCodeAction: TAction
      Category = 'View'
      AutoCheck = True
      GroupIndex = 1
      Hint = 'Show/Hide Code Editor'
      ImageIndex = 31
      OnExecute = ShowCodeActionExecute
    end
    object ShowPreviewAction: TAction
      Category = 'Run'
      Caption = 'ShowPreviewAction'
      ImageIndex = 56
      OnExecute = ShowPreviewActionExecute
      OnUpdate = ShowPreviewActionUpdate
    end
    object CollapseNodeAction: TAction
      Category = 'Edit'
      Caption = 'Collapse childrens'
      Hint = 'Collapse childrens of selected node'
      ImageIndex = 27
      OnExecute = CollapseNodeActionExecute
      OnUpdate = SelectedNodeActionUpdate
    end
    object ExpandNodeAction: TAction
      Category = 'Edit'
      Caption = 'Expand childrens'
      Hint = 'Expand childrens of selected node'
      ImageIndex = 28
      OnExecute = ExpandNodeActionExecute
      OnUpdate = SelectedNodeActionUpdate
    end
    object SearchAction: TAction
      Category = 'Edit'
      Caption = 'Search ...'
      Hint = 'Search'
      ImageIndex = 58
      ShortCut = 16454
      OnExecute = SearchActionExecute
      OnUpdate = SearchActionUpdate
    end
    object SearchAgainAction: TAction
      Category = 'Edit'
      Caption = 'Repeat search'
      Hint = 'Repeat last search'
      ImageIndex = 59
      ShortCut = 114
      OnExecute = SearchAgainActionExecute
      OnUpdate = SearchAgainActionUpdate
    end
    object ValidateAction: TAction
      Category = 'Config'
      Caption = 'Validate Metadata'
      Hint = 'Detect and report errors in this metadata file'
      ImageIndex = 15
      OnExecute = ValidateActionExecute
      OnUpdate = ValidateActionUpdate
    end
    object SaveAction: TAction
      Category = 'Edit'
      Caption = 'Save'
      Hint = 'Save changes to file'
      ImageIndex = 23
      OnExecute = SaveActionExecute
      OnUpdate = SaveUndoActionUpdate
    end
    object ReloadAction: TAction
      Category = 'Edit'
      Caption = 'Undo/Reload'
      Hint = 'Undo changes (reload from disk)'
      ImageIndex = 66
      OnExecute = ReloadActionExecute
      OnUpdate = SaveUndoActionUpdate
    end
  end
end
