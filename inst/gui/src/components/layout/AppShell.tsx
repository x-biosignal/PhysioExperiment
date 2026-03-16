import { ReactNode } from 'react'
import { useAppStore } from '@/stores/appStore'
import { Sidebar } from './Sidebar'
import { Header } from './Header'
import { StatusBar } from './StatusBar'
import { cn } from '@/lib/utils'

interface AppShellProps {
  children: ReactNode
}

export function AppShell({ children }: AppShellProps) {
  const sidebarCollapsed = useAppStore((state) => state.sidebarCollapsed)

  return (
    <div className="flex h-screen flex-col bg-background">
      <Header />
      <div className="flex flex-1 overflow-hidden">
        <Sidebar />
        <main
          className={cn(
            'flex-1 overflow-auto transition-all duration-300',
            sidebarCollapsed ? 'ml-16' : 'ml-64'
          )}
        >
          <div className="h-full">
            {children}
          </div>
        </main>
      </div>
      <StatusBar />
    </div>
  )
}
