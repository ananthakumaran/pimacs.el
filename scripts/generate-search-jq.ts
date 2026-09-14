import { readFileSync, writeFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const scriptDir = path.dirname(fileURLToPath(import.meta.url));
const jqFile = path.join(scriptDir, "pimacs-search.jq");
const elispFile = path.join(scriptDir, "..", "pimacs-search.el");
const filterDefinition =
  /\(defconst pimacs-search--jq-filter\n {2}"(?:\\[\s\S]|[^"\\])*"\)/g;
const jqFilter = readFileSync(jqFile, "utf8");
const generatedDefinition = `(defconst pimacs-search--jq-filter
  "${jqFilter.replaceAll("\\", "\\\\").replaceAll('"', '\\"')}")`;
const elisp = readFileSync(elispFile, "utf8");
const definitions = elisp.match(filterDefinition) ?? [];

if (definitions.length !== 1) {
  throw new Error(
    `Expected one pimacs-search--jq-filter definition in ${elispFile}, found ${definitions.length}`,
  );
}

const updatedElisp = elisp.replace(filterDefinition, generatedDefinition);
if (updatedElisp !== elisp) {
  writeFileSync(elispFile, updatedElisp);
}
