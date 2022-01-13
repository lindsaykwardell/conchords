<script setup lang="ts">
import "./assets/index.css";
import { Elm } from "./src/Main.elm";
import elmBridge from 'elm-vue-bridge'

const Main = elmBridge(Elm, "Main");

const music = await useFetch("/api/music")
</script>

<template>
  <div>
    <Main :flags="music.data.value" />
  </div>
</template>
