inherited ListControllerDynalistFilterNodeFrame: TListControllerDynalistFilterNodeFrame
  inherited ClientPanel: TPanel
    inherited DesignPanel: TPanel
      inherited ListFilterGroupBox: TGroupBox
        Height = 101
        Caption = 'DynaList'
        ExplicitHeight = 101
      end
      object ClientBottomPanel: TPanel
        Left = 0
        Top = 101
        Width = 451
        Height = 234
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        OnResize = ClientBottomPanelResize
        ExplicitTop = 241
        ExplicitHeight = 94
        object Splitter1: TSplitter
          Left = 0
          Top = 105
          Width = 451
          Height = 4
          Cursor = crVSplit
          Align = alTop
          ExplicitTop = 89
        end
        object ExpressionTemplateGroupBox: TGroupBox
          Left = 0
          Top = 0
          Width = 451
          Height = 105
          Align = alTop
          Caption = 'ExpressionTemplate'
          TabOrder = 0
        end
        object CommandTextGroupBox: TGroupBox
          Left = 0
          Top = 109
          Width = 451
          Height = 125
          Align = alClient
          Caption = 'CommandText'
          TabOrder = 1
          ExplicitLeft = 16
          ExplicitTop = 184
          ExplicitHeight = 89
        end
      end
    end
  end
  inherited ActionList: TActionList
    Left = 304
    Top = 64
  end
end
