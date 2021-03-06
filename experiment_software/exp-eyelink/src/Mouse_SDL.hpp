#ifndef SBX_MOUSE_SDL_INCLUDED
#define SBX_MOUSE_SDL_INCLUDED

#include "global.hpp"
#include "InputDev.hpp"
#include "Display_SDL.hpp"
#include "StimulusBmp.hpp"
#include <vector>

using std::vector;

class Template;

class MousePoint {
public:
  Uint32 m_ms;
  unsigned int m_x;
  unsigned int m_y;
  MousePoint();
  MousePoint(Uint32 ms, int x, int y);
  const char * QueryStr(long idResp);

  MousePoint( const MousePoint& source ):
    m_ms( source.m_ms ),
    m_x(  source.m_x ),
    m_y(  source.m_y ) {};

  MousePoint& operator=( const MousePoint& source )
  {
    m_ms = source.m_ms;
    m_x  = source.m_x;
    m_y  = source.m_y;
  }
};

typedef vector<MousePoint> MPvec;

class Mouse_SDL : public InputDev {
protected:
  MPvec m_vPts;
  void NewPt(Uint32 ms, int x, int y);
  bool m_bFirst;
  StimulusBmp * m_pCursor;
  SDL_Surface * m_pOld;
  //Template * m_pTemplate;
  SDL_Rect m_rect;
  SDL_Rect m_rectOld;
  bool m_bDraw;
  unsigned int m_xLast;
  unsigned int m_yLast;

public:
  Mouse_SDL(unsigned long idDev = SBX_MOUSE_DEV, bool bDraw = 1, unsigned int xHome = 512, unsigned int yHome = 384);
  ~Mouse_SDL();
  virtual void Prepare();
  virtual void Start();
  virtual void HandleEvent(SDL_Event * pEvt);
  virtual void Cleanup();
  MPvec inline GetMouseData() { return m_vPts; }
  void DrawCursor(int old_x, int old_y);
  inline unsigned int GetX() {return m_xLast;}
  inline unsigned int GetY() {return m_yLast;}
  static unsigned int s_x0;
  static unsigned int s_y0;
  static unsigned int s_xHome;
  static unsigned int s_yHome;
  inline void SetDraw(bool bDraw) { m_bDraw = bDraw; }
  void SetHome(const char * pcHome);
};

extern Display_SDL * g_pDisplay;

#endif
