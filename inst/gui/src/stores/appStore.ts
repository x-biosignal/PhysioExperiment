import { create } from 'zustand'
import { devtools, persist } from 'zustand/middleware'

export type Theme = 'light' | 'dark' | 'system'

interface AppState {
  // Session
  sessionId: string | null
  isConnected: boolean

  // UI State
  theme: Theme
  language: 'ja' | 'en'
  sidebarCollapsed: boolean
  activePanel: string

  // Notifications
  notifications: Notification[]

  // Actions
  setSessionId: (id: string | null) => void
  setConnected: (connected: boolean) => void
  setTheme: (theme: Theme) => void
  setLanguage: (language: 'ja' | 'en') => void
  toggleSidebar: () => void
  setActivePanel: (panel: string) => void
  addNotification: (notification: Omit<Notification, 'id' | 'timestamp'>) => void
  removeNotification: (id: string) => void
}

interface Notification {
  id: string
  type: 'info' | 'success' | 'warning' | 'error'
  title: string
  message: string
  duration?: number
  timestamp: number
}

export const useAppStore = create<AppState>()(
  devtools(
    persist(
      (set) => ({
        // Initial state
        sessionId: null,
        isConnected: false,
        theme: 'system',
        language: 'ja',
        sidebarCollapsed: false,
        activePanel: 'dashboard',
        notifications: [],

        // Actions
        setSessionId: (id) => set({ sessionId: id }),
        setConnected: (connected) => set({ isConnected: connected }),
        setTheme: (theme) => set({ theme }),
        setLanguage: (language) => set({ language }),
        toggleSidebar: () => set((state) => ({ sidebarCollapsed: !state.sidebarCollapsed })),
        setActivePanel: (panel) => set({ activePanel: panel }),

        addNotification: (notification) =>
          set((state) => ({
            notifications: [
              ...state.notifications,
              { ...notification, id: crypto.randomUUID(), timestamp: Date.now() },
            ],
          })),

        removeNotification: (id) =>
          set((state) => ({
            notifications: state.notifications.filter((n) => n.id !== id),
          })),
      }),
      {
        name: 'physio-app-store',
        partialize: (state) => ({
          theme: state.theme,
          sidebarCollapsed: state.sidebarCollapsed,
        }),
      }
    )
  )
)
