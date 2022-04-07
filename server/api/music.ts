import type { IncomingMessage, ServerResponse } from "http";
import prismaClient from "@prisma/client";
import { useBody, useQuery } from "h3";

type Music = {
  title: string;
  composer: string;
  publisher: string;
  parts: string;
  qty: number;
};

const prisma = new prismaClient.PrismaClient();

export default async (req: IncomingMessage, res: ServerResponse) => {
  // if (req.method !== "POST") {
  //   res.statusCode = 404;
  //   res.end();
  //   return;
  // }

  switch (req.method) {
    case "GET":
      return get(req, res);
    case "POST":
      return post(req, res);
    default:
      res.statusCode = 404;
      res.end();
      return;
  }
};

async function get(req: IncomingMessage, res: ServerResponse) {
  return prisma.music.findMany();
}

async function post(req: IncomingMessage, res: ServerResponse) {
  const body = await useBody<Music | Music[]>(req);
  if (!Array.isArray(body)) {
    if (!!body.title) {
      return prisma.music.create({
        data: {
          ...body,
        },
      });
    } else {
      res.statusCode = 400;
      res.end();
      return;
    }
  } else {
    await prisma.music.createMany({
      data: body
        .filter((music) => !!music.title)
        .map((music) => ({
          ...music,
        })),
    });

    return prisma.music.findMany();
  }
}
