// @ts-check
import { defineConfig } from "astro/config";
import starlight from "@astrojs/starlight";
import tailwindcss from "@tailwindcss/vite";
import starlightBlog from "starlight-blog";
import { unified } from "@astrojs/markdown-remark";
import remarkMath from "remark-math";
import rehypeKatex from "rehype-katex";

// https://astro.build/config
export default defineConfig({
  site: "https://shixiongfei.com",

  markdown: {
    processor: unified({
      remarkPlugins: [remarkMath],
      rehypePlugins: [rehypeKatex],
    }),
  },

  integrations: [
    starlight({
      plugins: [
        starlightBlog({
          navigation: "header-start",
          authors: {
            shixiongfei: {
              name: "shixiongfei",
              title: "Create something...",
              picture: "./src/assets/head.jpg",
              url: "https://github.com/shixiongfei",
            },
          },
        }),
      ],
      title: "Xiongfei Shi",
      description:
        "Xiongfei Shi's homepage and blog, record and share life and technology.",
      logo: {
        src: "./src/assets/houston.webp",
      },
      customCss: ["./src/styles/global.css"],
      components: {
        SocialIcons: "./src/components/SocialIcons.astro",
      },
      social: [
        {
          icon: "twitter",
          label: "Twitter",
          href: "https://twitter.com/xiongfei_shi",
        },
        {
          icon: "github",
          label: "GitHub",
          href: "https://github.com/shixiongfei",
        },
      ],
      sidebar: [],
    }),
  ],

  vite: {
    plugins: [tailwindcss()],
  },
});
