unit u_resourcestring;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
sDelete='Delete';
sOverwrite='Overwrite';
sReplace='Replace';
sCancel='Cancel';
sContinue='Continue';
sOk='Ok';

sSystem='System';
sProject='Project';


sEnterTheNewName='Enter the new name:';
sIfYouLeaveChangeWillBeLost='If you leave, changes will be lost.'+LineEnding+'Continue ?';
sLeaveWithoutSaving='Leave without saving';

sGroups='Groups';
sDescription='Description';
sThereIsntGroupDesc='There isn''t a description for this group';
sThereIsntLevelDesc='There isn''t a description for this level';
sEnterANameForTheGroup='Enter a name for the group:';
sTheGroupxxAlreadyExists='The group %s already exists. Please retry with another name';
sDeleteTheGroupAndItsLevels='Delete the group and its levels ? (this action is irreversible)';
sTheLevelxxAlreadyExists='The level %s already exists. Please retry with another name';
sDeleteThisLevel='Delete this level ? (this action is irreversible)';


sAxxxNamedAlreadyExists='A %s named %s already exists. Please, try with another name';
sAxxxAlreadyExistsWouldYouLikeToReplaceIt='a %s named %s already exists in the bank.'+LineEnding+
                'Would you like to replace it ?';
sDeleteThisxxx='Delete this %s ?';
sFont='font';
sPanel='panel';
sPath='path';
sScenario='scenario';

sFontDescriptor='Font descriptor';
sOverwriteTheSelectedFont='Overwrite the selected font ?';
sAvailableFonts='Available fonts';

sPanels='Panels';
sPaths='Paths';

implementation

end.


