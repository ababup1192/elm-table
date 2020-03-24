import { Elm } from "./Main.elm";
import { rows } from "./data/data";

const app = Elm.Main.init({ node: document.getElementById("main"), flags: rows });
