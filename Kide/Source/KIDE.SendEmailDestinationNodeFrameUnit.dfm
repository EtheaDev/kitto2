inherited SendEmailDestinationNodeFrame: TSendEmailDestinationNodeFrame
  Width = 387
  ExplicitWidth = 387
  inherited ClientPanel: TPanel
    Width = 387
    inherited DesignPanel: TPanel
      Width = 387
      object DestinationEdit: TLabeledEdit
        Left = 6
        Top = 19
        Width = 375
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 72
        EditLabel.Height = 13
        EditLabel.Caption = 'DestinationEdit'
        TabOrder = 0
      end
      object RecipientsGroupBox: TGroupBox
        Left = 6
        Top = 46
        Width = 375
        Height = 247
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Recipients'
        TabOrder = 1
      end
    end
  end
end
