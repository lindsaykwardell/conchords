<script setup lang="ts">
import "./assets/index.css";
import { Elm } from "./src/Main.elm";
import elmBridge from "elm-vue-bridge";

const Main = elmBridge(Elm, "Main");

const music = await useFetch("/api/music");
</script>

<template>
  <link
    href="https://fonts.googleapis.com/css2?family=Oswald&display=swap"
    rel="stylesheet"
  />
  <Main :flags="music.data.value" />
</template>
