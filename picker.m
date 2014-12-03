#import <Cocoa/Cocoa.h>

const char *macSelectDir(char *startIn) {
  NSOpenPanel *panel = [NSOpenPanel openPanel];
  [panel setCanChooseFiles:NO];
  [panel setCanChooseDirectories:YES];
  [panel setAllowsMultipleSelection:NO];

  if (startIn) {
    NSString *str = [[NSString alloc] initWithUTF8String: startIn];
    [panel setDirectoryURL:[NSURL fileURLWithPath:str]];
    [str release];
  }

  NSInteger clicked = [panel runModal];

  const char *original = NULL;
  if (clicked == NSFileHandlingPanelOKButton) {
    for (NSURL *url in [panel URLs]) {
      original = [url fileSystemRepresentation];
      break;
    }
  }
  char *copied = NULL;
  if (original) {
    copied = malloc(strlen(original) + 1);
    strcpy(copied, original);
  }

  [panel release];
  return copied;
}
