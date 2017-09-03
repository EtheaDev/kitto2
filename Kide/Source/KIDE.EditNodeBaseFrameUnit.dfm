inherited EditNodeBaseFrame: TEditNodeBaseFrame
  Width = 589
  Height = 299
  HelpType = htKeyword
  OnEnter = FrameEnter
  ExplicitWidth = 589
  ExplicitHeight = 299
  object ClientPanel: TPanel
    Left = 0
    Top = 0
    Width = 589
    Height = 299
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object DesignPanel: TPanel
      Left = 0
      Top = 0
      Width = 589
      Height = 299
      Align = alClient
      BevelOuter = bvNone
      Ctl3D = True
      ParentCtl3D = False
      TabOrder = 0
    end
  end
  object ActionList: TActionList
    Images = MainDataModule.Images
    OnUpdate = ActionListUpdate
    Left = 192
    Top = 112
  end
end
