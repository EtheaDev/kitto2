inherited CodeEditorFrame: TCodeEditorFrame
  Width = 357
  Height = 268
  ParentDoubleBuffered = False
  ParentShowHint = False
  ExplicitWidth = 357
  ExplicitHeight = 268
  object ErrorLabel: TLabel
    Left = 0
    Top = 252
    Width = 357
    Height = 16
    Align = alBottom
    AutoSize = False
    Caption = 'ErrorLabel'
    Color = 13421823
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    Transparent = False
    Visible = False
    ExplicitTop = 253
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 357
    Height = 26
    AutoSize = True
    ButtonHeight = 26
    ButtonWidth = 26
    Images = MainDataModule.Images
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object ApplyActionToolButton: TToolButton
      Left = 0
      Top = 0
      Action = ApplyAction
    end
    object CancelActionToolButton: TToolButton
      Left = 26
      Top = 0
      Action = CancelAction
    end
    object Sep1ToolButton: TToolButton
      Left = 52
      Top = 0
      Width = 8
      Caption = 'Sep1ToolButton'
      ImageIndex = 1
      Style = tbsSeparator
    end
    object UndoActionToolButton: TToolButton
      Left = 60
      Top = 0
      Action = UndoAction
    end
    object RedoActionToolButton: TToolButton
      Left = 86
      Top = 0
      Action = RedoAction
    end
    object Sep2ToolButton: TToolButton
      Left = 112
      Top = 0
      Width = 8
      Caption = 'Sep2ToolButton'
      ImageIndex = 29
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
    object SearchAndReplaceToolButton: TToolButton
      Left = 172
      Top = 0
      Action = SearchAndReplaceAction
    end
    object Sep3ToolButton: TToolButton
      Left = 198
      Top = 0
      Width = 8
      ImageIndex = 61
      Style = tbsSeparator
    end
    object OptionsActionToolButton: TToolButton
      Left = 206
      Top = 0
      Action = OptionsAction
    end
    object EditorColorOptionsButton: TToolButton
      Left = 232
      Top = 0
      Action = EditorColorOptionsAction
    end
  end
  object ActionList: TActionList
    Images = MainDataModule.Images
    Left = 192
    Top = 112
    object UndoAction: TAction
      Caption = 'UndoAction'
      Enabled = False
      Hint = 'Undo last change'
      ImageIndex = 27
      OnExecute = UndoActionExecute
      OnUpdate = UndoActionUpdate
    end
    object RedoAction: TAction
      Caption = 'RedoAction'
      Enabled = False
      Hint = 'Redo last undone change'
      ImageIndex = 28
      OnExecute = RedoActionExecute
      OnUpdate = RedoActionUpdate
    end
    object OptionsAction: TAction
      Caption = 'Editor Options...'
      Hint = 'Editor Options...'
      ImageIndex = 29
      OnExecute = OptionsActionExecute
    end
    object ApplyAction: TAction
      Caption = 'Apply'
      Enabled = False
      Hint = 'Apply Edits'
      ImageIndex = 32
      ShortCut = 16467
      OnExecute = ApplyActionExecute
      OnUpdate = ApplyActionUpdate
    end
    object CancelAction: TAction
      Caption = 'Cancel'
      Enabled = False
      Hint = 'Cancel Edits'
      ImageIndex = 33
      OnExecute = CancelActionExecute
      OnUpdate = CancelActionUpdate
    end
    object EditorColorOptionsAction: TAction
      Caption = 'Editor Color Options...'
      Hint = 'Editor Color Options...'
      ImageIndex = 57
      OnExecute = EditorColorOptionsActionExecute
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
    object SearchAndReplaceAction: TAction
      Category = 'Edit'
      Caption = 'Replace ...'
      Hint = 'Search and Replace'
      ImageIndex = 60
      ShortCut = 16466
      OnExecute = SearchAndReplaceActionExecute
      OnUpdate = SearchAndReplaceActionUpdate
    end
  end
end
