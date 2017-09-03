inherited PairsValuesFrame: TPairsValuesFrame
  Width = 427
  Height = 245
  OnResize = FrameResize
  ExplicitWidth = 427
  ExplicitHeight = 245
  inherited ClientPanel: TPanel
    Top = 26
    Width = 427
    Height = 219
    TabOrder = 2
    ExplicitTop = 30
    ExplicitWidth = 427
    ExplicitHeight = 215
    inherited DesignPanel: TPanel
      Width = 427
      Height = 219
      ExplicitWidth = 427
      ExplicitHeight = 215
    end
  end
  object ToolBar: TToolBar [1]
    Left = 0
    Top = 0
    Width = 427
    Height = 26
    AutoSize = True
    ButtonHeight = 26
    ButtonWidth = 26
    Images = MainDataModule.Images
    TabOrder = 0
    object AddRowActionToolButton: TToolButton
      Left = 0
      Top = 0
      Action = AddRowAction
    end
    object DeleteRowActionToolButton: TToolButton
      Left = 26
      Top = 0
      Action = DeleteRowAction
    end
    object MoveDownActionToolButton: TToolButton
      Left = 52
      Top = 0
      Action = MoveDownAction
    end
    object MoveUpActionToolButton: TToolButton
      Left = 78
      Top = 0
      Action = MoveUpAction
    end
  end
  object PairsValueListEditor: TValueListEditor [2]
    Left = 0
    Top = 26
    Width = 427
    Height = 219
    Align = alClient
    KeyOptions = [keyEdit, keyAdd, keyDelete]
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing, goTabs, goThumbTracking]
    TabOrder = 1
    TitleCaptions.Strings = (
      'Node'
      'Value')
    OnSelectCell = PairsValueListEditorSelectCell
    OnStringsChange = PairsValueListEditorStringsChange
    ExplicitTop = 30
    ExplicitHeight = 215
    ColWidths = (
      150
      271)
  end
  inherited ActionList: TActionList
    Left = 24
    Top = 72
  end
  object PairsActionList: TActionList
    Images = MainDataModule.Images
    Left = 312
    Top = 48
    object AddRowAction: TAction
      Category = 'Edit'
      Caption = 'Add Row'
      Hint = 'Add a line'
      ImageIndex = 51
      OnExecute = AddRowActionExecute
    end
    object DeleteRowAction: TAction
      Category = 'Edit'
      Caption = 'Delete Row'
      Hint = 'Delete current line'
      ImageIndex = 47
      OnExecute = DeleteRowActionExecute
    end
    object MoveDownAction: TAction
      Category = 'Edit'
      Caption = 'Move Down'
      Hint = 'Move down current line'
      ImageIndex = 53
      OnExecute = MoveDownActionExecute
      OnUpdate = MoveDownActionUpdate
    end
    object MoveUpAction: TAction
      Category = 'Edit'
      Caption = 'Move Up'
      Hint = 'Move up current line'
      ImageIndex = 54
      OnExecute = MoveUpActionExecute
      OnUpdate = MoveUpActionUpdate
    end
  end
end
