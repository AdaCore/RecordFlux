#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <stdint.h>

ssize_t c_send(int fd, uint32_t addr, const void *buf, size_t len)
{
  struct sockaddr_in saddr;
  saddr.sin_family = AF_INET;
  saddr.sin_port = IPPROTO_RAW;
  saddr.sin_addr.s_addr = htonl(addr);
  return sendto(fd, buf, len, 0, (struct sockaddr *)&saddr, sizeof(saddr));
}
