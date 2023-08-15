import type { EChartsOption } from "echarts";

import type { VisualizationSettings } from "metabase-types/api";

type EChartsEventHandler = {
  // TODO better types
  eventName: string;
  query?: string;
  handler: (event: any) => void;
};

type ZREventHandler = {
  // TODO better types
  eventName: string;
  handler: (event: any) => void;
};

export type EChartsConfig = {
  option: EChartsOption;
  eventHandlers: EChartsEventHandler[];
  zrEventHandlers: ZREventHandler[];
};

type EChartsMixin = (params: {
  chartType: any;
  data: any;
  settings: VisualizationSettings;
  option: EChartsOption;
}) => {
  option: EChartsOption;
  eventHandlers?: EChartsEventHandler[];
  zrEventHandlers?: ZREventHandler[];
};

export const lineSeriesMixin: EChartsMixin = ({
  chartType,
  data,
  settings,
  option,
}) => {
  return {
    option: {
      xAxis: {
        type: "category",
        data: ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"],
      },
      yAxis: {
        type: "value",
      },
      series: [
        {
          id: "data",
          data: [150, 230, 224, 218, 135, 147, 260],
          type: "line",
        },
      ],
    },
  };
};

export const smoothSettingMixin: EChartsMixin = ({ settings, option }) => {
  if (Array.isArray(option?.series)) {
    option.series.forEach(series => {
      series.smooth = settings.smooth;
    });
  }

  return { option };
};

export const clickActionsMixin: EChartsMixin = ({ option }) => {
  return {
    option,
    eventHandlers: [
      { eventName: "click", handler: e => console.log("clicked", e) },
    ],
  };
};

export function useEChartsConfig({
  chartType,
  data,
  settings,
  mixins,
}: {
  chartType: any;
  data: any;
  settings: VisualizationSettings;
  mixins: EChartsMixin[];
}) {
  const emptyConfig: EChartsConfig = {
    option: {},
    eventHandlers: [],
    zrEventHandlers: [],
  };

  return mixins.reduce((currentConfig: EChartsConfig, currentMixin) => {
    const next = currentMixin({
      chartType,
      data,
      settings,
      option: currentConfig.option,
    });

    return {
      option: next.option,
      eventHandlers: [
        ...currentConfig.eventHandlers,
        ...(next.eventHandlers ?? []),
      ],
      zrEventHandlers: [
        ...currentConfig.zrEventHandlers,
        ...(next.zrEventHandlers ?? []),
      ],
    };
  }, emptyConfig);
}