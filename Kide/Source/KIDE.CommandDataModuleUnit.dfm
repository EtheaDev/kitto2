object CommandDataModule: TCommandDataModule
  OldCreateOrder = False
  Height = 150
  Width = 215
  object PopupActionBar: TPopupActionBar
    Images = MainDataModule.Images
    MenuAnimation = [maLeftToRight, maTopToBottom]
    Left = 64
    Top = 56
    object AddChildActionMenuItem: TMenuItem
      Action = AddChildAction
    end
    object DeleteNodeActionMenuItem: TMenuItem
      Action = DeleteNodeAction
    end
  end
  object ActionList: TActionList
    Images = MainDataModule.Images
    Left = 136
    Top = 56
    object AddChildAction: TAction
      Caption = 'Add Child'
      ImageIndex = 43
      OnExecute = AddChildActionExecute
      OnUpdate = AddChildActionUpdate
    end
    object DeleteNodeAction: TAction
      Caption = 'Delete Node'
      ImageIndex = 44
      OnExecute = DeleteNodeActionExecute
      OnUpdate = DeleteNodeActionUpdate
    end
  end
end
