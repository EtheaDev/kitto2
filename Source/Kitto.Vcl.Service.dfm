object KService: TKService
  OldCreateOrder = False
  AllowPause = False
  DisplayName = 'KittoService'
  OnShutdown = ServiceShutdown
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 150
  Width = 215
end
