import type { MantineThemeOverride } from "@mantine/core";
import { DEFAULT_Z_INDEX } from "metabase/components/Popover/constants";

export const getPopoverOverrides = (): MantineThemeOverride["components"] => ({
  Popover: {
    defaultProps: {
      radius: "sm",
      shadow: "md",
      zIndex: DEFAULT_Z_INDEX,
      withinPortal: true,
    },
    styles: () => ({
      dropdown: {
        padding: 0,
      },
    }),
  },
});