import { appendFileSync } from "node:fs";
import { spawn } from "node:child_process";
import { once } from "node:events";
import path from "node:path";
import { fileURLToPath } from "node:url";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { Type } from "typebox";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const tapesDir = path.join(__dirname, "../tapes");
const mode = process.env.FIXTURE_MODE || "replay";
const scenario = process.env.FIXTURE_SCENARIO || "default";

const binPath = path.join(__dirname, "../node_modules/.bin/proxay");
const logFile = "/tmp/proxay.log";

function initialize() {
  const proxay = spawn(binPath, [
    "--mode",
    mode,
    "--tapes-dir",
    tapesDir,
    "--default-tape",
    scenario,
    "--host",
    "http://100.70.125.96:11434",
    "--port",
    "5544",
  ]);

  proxay.stdout.on("data", (data) => {
    appendFileSync(logFile, `[stdout] ${data.toString()}`);
  });

  proxay.stderr.on("data", (data) => {
    appendFileSync(logFile, `[stderr] ${data.toString()}`);
  });

  proxay.on("exit", (code) => {
    appendFileSync(logFile, `[exit] code=${code}\n`);
  });

  return proxay;
}

export default function (pi: ExtensionAPI) {
  const proxay = initialize();

  [
    "exit",
    "SIGINT",
    "SIGUSR1",
    "SIGUSR2",
    "uncaughtException",
    "SIGTERM",
  ].forEach((eventType) => {
    process.on(eventType, () => {
      appendFileSync(logFile, `[pi](${eventType}) stopping proxay\n`);
      proxay.kill();
    });
  });

  pi.registerCommand("rpc-input", {
    description: "Prompt for text input (ctx.ui.input)",
    handler: async (_args, ctx) => {
      const value = await ctx.ui.input("Enter a value", "type something...");
      ctx.ui.notify(`Input result: ${value ?? "cancelled"}`, "info");
    },
  });

  pi.registerCommand("rpc-confirm", {
    description: "Prompt for confirmation (ctx.ui.confirm)",
    handler: async (_args, ctx) => {
      const confirmed = await ctx.ui.confirm(
        "Continue?",
        "Do you want to proceed?",
      );
      ctx.ui.notify(`Confirmed: ${confirmed}`, "info");
    },
  });

  pi.registerCommand("rpc-select", {
    description: "Prompt for selection (ctx.ui.select)",
    handler: async (_args, ctx) => {
      const value = await ctx.ui.select("Pick an option", [
        "Option A",
        "Option B",
        "Option C",
      ]);
      ctx.ui.notify(`Selected: ${value ?? "cancelled"}`, "info");
    },
  });

  pi.registerCommand("rpc-notify", {
    description: "Send notifications (ctx.ui.notify)",
    handler: async (_args, ctx) => {
      ctx.ui.notify("Info notification", "info");
      ctx.ui.notify("Warning notification", "warning");
      ctx.ui.notify("Error notification", "error");
    },
  });

  pi.registerCommand("rpc-editor", {
    description: "Open editor (ctx.ui.editor)",
    handler: async (_args, ctx) => {
      const value = await ctx.ui.editor("Edit some text", "prefilled text");
      ctx.ui.notify(`Editor result: ${value ?? "cancelled"}`, "info");
    },
  });

  pi.registerCommand("rpc-set-editor-text", {
    description: "Set editor text (ctx.ui.setEditorText)",
    handler: async (_args, ctx) => {
      ctx.ui.setEditorText("hello from extension");
      ctx.ui.notify("Editor text set", "info");
    },
  });

  pi.registerCommand("rpc-set-widget", {
    description: "Set widgets above and below editor (ctx.ui.setWidget)",
    handler: async (_args, ctx) => {
      ctx.ui.setWidget("rpc-widget-above", ["Widget line 1", "Widget line 2"]);
      ctx.ui.setWidget("rpc-widget-below", ["Widget line 3", "Widget line 4"], {
        placement: "belowEditor",
      });
      ctx.ui.notify("Widget set", "info");
    },
  });

  pi.registerCommand("rpc-set-status", {
    description: "Set status (ctx.ui.setStatus)",
    handler: async (_args, ctx) => {
      ctx.ui.setStatus("rpc-status-a", "Status A value");
      ctx.ui.setStatus("rpc-status-b", "Status B value");
      ctx.ui.notify("Status set", "info");
    },
  });

  pi.registerCommand("rpc-set-title", {
    description: "Set title (ctx.ui.setTitle)",
    handler: async (_args, ctx) => {
      ctx.ui.setTitle("Custom Title");
      ctx.ui.notify("Title set", "info");
    },
  });

  pi.registerProvider("fixture", {
    api: "openai-completions",
    baseUrl: "http://127.0.0.1:5544/v1",
    apiKey: "ollama",
    models: [
      {
        id: "qwen3.5:0.8b",
        name: "Qwen 3.5:0.8b",
        reasoning: true,
        input: ["text"],
        cost: {
          input: 0,
          output: 0,
          cacheRead: 0,
          cacheWrite: 0,
        },
        contextWindow: 200000,
        maxTokens: 100000,
      },
    ],
  });
}
