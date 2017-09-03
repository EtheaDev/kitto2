inherited RuleNodeFrame: TRuleNodeFrame
  Width = 449
  Height = 302
  HelpKeyword = 'Config_Databases'
  ExplicitWidth = 449
  ExplicitHeight = 302
  inherited ClientPanel: TPanel
    Width = 449
    Height = 302
    ExplicitWidth = 671
    ExplicitHeight = 485
    inherited DesignPanel: TPanel
      Width = 449
      Height = 302
      ExplicitWidth = 671
      ExplicitHeight = 485
      object RuleNameEdit: TLabeledEdit
        Left = 8
        Top = 24
        Width = 217
        Height = 21
        EditLabel.Width = 51
        EditLabel.Height = 13
        EditLabel.Caption = 'Rule Name'
        TabOrder = 0
      end
      object ParamsGroupBox: TGroupBox
        Left = 8
        Top = 51
        Width = 432
        Height = 239
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Params'
        TabOrder = 1
        ExplicitWidth = 654
        ExplicitHeight = 422
      end
      object RuleValueEdit: TLabeledEdit
        Left = 231
        Top = 24
        Width = 209
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 50
        EditLabel.Height = 13
        EditLabel.Caption = 'Rule Value'
        TabOrder = 2
      end
    end
  end
end
