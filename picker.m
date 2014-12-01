#import <Cocoa/Cocoa.h>

const char *macSelectDir() {
  NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];

  NSOpenPanel *panel = [NSOpenPanel openPanel];
  [panel setCanChooseFiles:NO];
  [panel setCanChooseDirectories:YES];
  [panel setAllowsMultipleSelection:NO];

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

  [pool drain];
  return copied;
}
