import { t } from "ttag";
import { CartesianChart } from "metabase/visualizations/visualizations/CartesianChart";
import {
  getDefaultSize,
  getMinSize,
} from "metabase/visualizations/shared/utils/sizes";
import { GRAPH_GOAL_SETTINGS } from "../../lib/settings/goal";
import {
  GRAPH_DATA_SETTINGS,
  GRAPH_TREND_SETTINGS,
  GRAPH_COLORS_SETTINGS,
  GRAPH_AXIS_SETTINGS,
  GRAPH_DISPLAY_VALUES_SETTINGS,
  STACKABLE_SETTINGS,
} from "../../lib/settings/graph";
import type {
  ComputedVisualizationSettings,
  VisualizationProps,
} from "../../types";

Object.assign(BarChart, {
  uiName: t`Bar`,
  identifier: "bar",
  iconName: "bar",
  noun: t`bar chart`,
  minSize: getMinSize("bar"),
  defaultSize: getDefaultSize("bar"),
  settings: {
    ...STACKABLE_SETTINGS,
    ...GRAPH_GOAL_SETTINGS,
    ...GRAPH_TREND_SETTINGS,
    ...GRAPH_COLORS_SETTINGS,
    ...GRAPH_AXIS_SETTINGS,
    ...GRAPH_DISPLAY_VALUES_SETTINGS,
    ...GRAPH_DATA_SETTINGS,
  },
  onDisplayUpdate: (settings: ComputedVisualizationSettings) => {
    if (settings["stackable.stack_display"]) {
      settings["stackable.stack_display"] = "bar";
    }
    return settings;
  },
});

export function BarChart(props: VisualizationProps) {
  return <CartesianChart {...props} />;
}
