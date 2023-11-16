import type { GridOption } from "echarts/types/dist/shared";
import type { Padding } from "metabase/visualizations/echarts/cartesian/option/types";
import type { ComputedVisualizationSettings } from "metabase/visualizations/types";
import { CHART_STYLE } from "metabase/visualizations/echarts/cartesian/option/style";

export const getChartGrid = (
  settings: ComputedVisualizationSettings,
): GridOption => {
  const padding: Padding = {
    top: CHART_STYLE.padding.y,
    left: CHART_STYLE.padding.x,
    bottom: CHART_STYLE.padding.y,
    right: CHART_STYLE.padding.x,
  };

  const hasYAxisName = settings["graph.y_axis.labels_enabled"];
  const yAxisNameTotalWidth =
    CHART_STYLE.axisName.size + 2 * CHART_STYLE.axisNameMargin;

  if (hasYAxisName) {
    padding.left += yAxisNameTotalWidth;
    padding.right += yAxisNameTotalWidth;
  }

  const hasXAxisName = settings["graph.x_axis.labels_enabled"];

  if (hasXAxisName) {
    padding.bottom += CHART_STYLE.axisName.size + CHART_STYLE.axisNameMargin;
  }

  return {
    containLabel: true,
    ...padding,
  };
};
