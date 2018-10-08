inherited FormControllerButtonDesignerFrame: TFormControllerButtonDesignerFrame
  Width = 396
  ExplicitWidth = 396
  inherited ClientPanel: TPanel
    Width = 396
    ExplicitWidth = 396
    inherited DesignPanel: TPanel
      Width = 396
      ExplicitWidth = 396
      object _Caption: TLabeledEdit
        Left = 6
        Top = 21
        Width = 139
        Height = 21
        EditLabel.Width = 37
        EditLabel.Height = 13
        EditLabel.Caption = 'Caption'
        TabOrder = 0
      end
      object _Tooltip: TLabeledEdit
        Left = 151
        Top = 21
        Width = 238
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 32
        EditLabel.Height = 13
        EditLabel.Caption = 'Tooltip'
        TabOrder = 1
      end
    end
  end
end
