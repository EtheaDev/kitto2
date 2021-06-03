{-------------------------------------------------------------------------------
   Copyright 2012-2021 Ethea S.r.l.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-------------------------------------------------------------------------------}
unit KIDE.OnGuardInit;

interface

uses
  SysUtils,
  OnGuard,
  EfOnGuard;

const

  //MasterKey for KIDE2 application
  AppKey : TKey = ($F6,$A9,$64,$BA,$72,$CB,$90,$CC,$89,$8D,$7E,$74,$32,$17,$8A,$F7);

implementation

uses
  Windows;

initialization
  // MasterKey for KIDE
  OnGuardInfo.InitMasterKey(AppKey,'KIDE2');
  OnGuardInfo.ContactInfo := 'support@ethea.it';

  // Serial Number Identification
  OnGuardInfo.SerialNumber := 2;

  // Identity Description of string required
  OnGuardInfo.IdentityKey.Description := 'Customer Name';

  // Machine Identifier mode
  OnGuardInfo.MachineInfoSet := [midSystem, midDrives];

  // Max Users Count
  OnGuardInfo.MaxConcurrentUsers := 0;

  // Storage (ini file)
  OnGuardInfo.StorageType := stIniFile;

finalization

end.
