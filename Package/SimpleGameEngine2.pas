{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit SimpleGameEngine2;

{$warn 5023 off : no warning about unused units}
interface

uses
  GDIPAPI, GDIPOBJ, GDIPUTIL, OpenAL, dglOpenGL, sgeErrorManager, 
  sgeCorePointerList, sgeResourceList, sgeWindow, SimpleGameEngine, 
  sgeCriticalSection, sgeDateUtils, sgeErrors, sgeFile, sgeFileUtils, sgeKeys, 
  sgeMemoryStream, sgeMetaInfoList, sgeOSPlatform, sgeSimpleCommand, 
  sgeSimpleContainer, sgeSimpleParameters, sgeStartParameters, sgeStringList, 
  sgeMathUtils, sgeSystemEvent, sgeSystemUtils, sgeThread, sgeTypes, 
  sgeControllerList, sgeEventControllers, sgeEventTimeEvent, sgeEventList, 
  sgeEventManager, sgeEventSubscriber, sgeEventSubscriberGroup, 
  sgeEventSubscriberGroupList, sgeEventSubscriberList, sgeEventWindow, 
  sgeExtensionControllers, sgeExtensionFileSystem, sgeExtensionGraphic, 
  sgeExtensionList, sgeExtensionGUI, sgeExtensionResourceList, 
  sgeExtensionShell, sgeExtensionSound, sgeExtensionStartParameters, 
  sgeExtensionVariables, sgeExtensionWindow, sgeGraphicAnimation, 
  sgeGraphicAnimationFrames, sgeGraphicColor, sgeGraphicElementAnimation, 
  sgeGraphicElementBase, sgeGraphicElementCircle, sgeGraphicElementLayer, 
  sgeGraphicElementLayerList, sgeGraphicElementList, 
  sgeGraphicElementRectangle, sgeGraphicElementRectangleGradient, 
  sgeGraphicElementSprite, sgeGraphicElementSpritePart, 
  sgeGraphicElementSpriteTile, sgeGraphicElementText, sgeGraphicFont, 
  sgeGraphicFPS, sgeGraphicSprite, sgeGraphicSpriteGDIPLoader, 
  sgeGraphicSpriteLoader, sgeGraphicUtils, sgeScreenFade, sgePackFileList, 
  sgePackFileReader, sgePackFileWriter, sgeSoundBuffer, sgeSoundBufferLoader, 
  sgeSoundBufferWavLoader, sgeSoundSource, sgeTemplateCollection, 
  sgeTemplateThreadSafeList, sgeSystemCursor, sgeSystemIcon, 
  sgeSystemTimeEvent, sgeSystemTrayIcon, sgeVariableBooleanNormal, 
  sgeVariableBooleanBase, sgeVariableBooleanProc, sgeVariableIntegerNormal, 
  sgeVariableIntegerBase, sgeVariableIntegerProc, sgeVariableList, 
  sgeVariableSingleNormal, sgeVariableSingleBase, sgeVariableSingleProc, 
  sgeVariableColorNormal, sgeVariableColorBase, sgeVariableColorProc, 
  sgeTemplateThreadSafeCollection, sgeShellCommand_System_Read, 
  sgeShellCommand, sgeShellCommandList, sgeCommandHistory, sgeSystemConsole, 
  sgeExtensionKeyCommand, sgeShellCommandQueue, sgeKeyCommandKeyboard, 
  sgeKeyCommandMouse, sgeKeyCommandJoystick, sgeKeyCommandJoystickButtonInfo, 
  sgeKeyCommandJoystickInfo, sgeKeyCommandJoystickAxisInfo, 
  sgeKeyCommandTypes, sgeKeyCommandJoystickPadInfo, 
  sgeKeyCommandJoystickAxisInfoTilt, sgeGraphicElementSpriteCashed, 
  sgeShellCommandParameterBase, sgeShellCommandParameterString, 
  sgeShellCommandParameterList, sgeShellCommandParameterEnum, 
  sgeShellCommandParameterFloat, sgeShellCommand_System_ReadLn, 
  sgeShellCommands, sgeShellCommand_System_ReadKey, 
  sgeShellCommand_Variable_Clear, sgeShellCommand_Dialog_Message, 
  sgeShellCommand_Variable_Delete, sgeShellCommand_Variable_Set, 
  sgeShellCommand_System_Sleep, sgeShellLine, sgeShellLineItem, 
  sgeShellLineList, sgeShellCommand_System_Write, 
  sgeShellCommand_System_Screenshot, sgeShellCommand_System_Label, 
  sgeStringUtils, sgeVariableEnumBase, sgeVariableEnumNormal, 
  sgeVariableStringProc, sgeVariables, sgeShellScript, sgeShellScriptList, 
  sgeShellCallStack, sgeShellCallStackItem, sgeShellCommand_System_Exit, 
  sgeShellCommand_System_Procedure, sgeShellCommand_System_Return, 
  sgeShellCommand_System_Execute, sgeShellCommand_System_Goto, 
  sgeShellCommand_System_Run, sgeShellCommandsUtils, 
  sgeShellCommand_System_Call, sgeShellCommand_Script_Clear, 
  sgeExtensionTimeEvent, sgeShellCommand_Script_Delete, 
  sgeShellCommand_Music_Random, sgeTimeEventList, sgeGUIElement, sgeGUIForm, 
  sgeGUIFormList, sgeEventMusicPlayer, sgeEventKeyboard, sgeEventMouse, 
  sgeGUIPropertySegmentOffset, sgeGUIPropertyIntRect, sgeGUIPropertyFloatRect, 
  sgeGUIPropertyFloatPoint, sgeGUIPropertyColor, sgeGUIPropertyScaleXY, 
  sgeGUIPropertyIntPoint, sgeGUIPropertyHorizontalAlign, 
  sgeGUIPropertyVerticalAlign, sgeGUIPropertySpriteRect, 
  sgeGUIPropertyDrawMethod, sgeGUITimer, sgeSystemFont, sgeJournal, 
  sgeGUIUtils, sgeGUISprite, sgeGUISpriteButton, sgeExtensionMusicPlayer, 
  sgeExtensionPackList, sgeMusicPlayerTrackList, sgeSystemGlobalAtom, 
  sgeMusicPlayerTaskBase, sgeMusicPlayerTaskList, sgeTemplateList, 
  sgeMusicPlayerTaskPlay, sgeMusicPlayerTaskStop, sgeMusicPlayerTaskVolume, 
  sgeVariableBooleanClass, sgeVariableColorClass, sgeVariableEnumProc, 
  sgeVariableIntegerClass, sgeVariableSingleClass, sgeVariableStringBase, 
  sgeVariableStringClass, sgeVariableStringNormal, sgeVariableEnumClass, 
  sgeEventGraphic, sgeGUIToggleSpriteButton, sgeShellCommand_Music_Next, 
  sgeShellCommand_Music_Play, sgeShellCommand_Music_Prev, 
  sgeShellCommand_CommandHistory_Load, sgeShellCommand_Command_List, 
  sgeGUIPropertyFont, sgeGUILabel, sgeGUIPropertyLabel, sgeGUIScrollBar, 
  sgeCursor, sgeExtensionCursor, sgeShellCommand_CommandHistory_Clear, 
  sgeShellCommand_CommandHistory_Save, sgeShellCommand_Music_Stop, 
  sgeShellCommand_Script_Load, sgeKeyCommandMouseAction, 
  sgeKeyCommandMouseActionList, sgeKeyCommandKeyboardAction, 
  sgeKeyCommandKeyboardActionList, sgeWindowsVersion, sgeMatch, sgeMatchMask, 
  sgeShellCommand_System_Stop, sgeGraphicElementFade, sgeCorePointerUtils, 
  sgeGUIPropertyStipple, sgeGUIPropertyBorder, sgeGUIRect, sgeGUISpriteView, 
  sgeExtensionScenes, sgeSceneBase, sgeSceneList, sgeViewBox;

implementation

end.
