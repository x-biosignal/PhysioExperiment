import { Routes, Route } from 'react-router-dom'
import { useAppStore } from '@/stores/appStore'
import { AppShell } from '@/components/layout/AppShell'
import { Dashboard } from '@/components/dashboard/Dashboard'
import { DataBrowser } from '@/components/data/DataBrowser'
import { AnalysisWorkspace } from '@/components/analysis/AnalysisWorkspace'
import { WorkflowAnalysis } from '@/components/analysis/WorkflowAnalysis'
import { SignalViewer } from '@/components/visualization/SignalViewer'
import { WorkflowBuilder } from '@/components/workflow/WorkflowBuilder'
import { Settings } from '@/components/settings/Settings'

function App() {
  const theme = useAppStore((state) => state.theme)

  return (
    <div className={theme === 'dark' ? 'dark' : ''}>
      <AppShell>
        <Routes>
          <Route path="/" element={<Dashboard />} />
          <Route path="/data" element={<DataBrowser />} />
          <Route path="/analysis" element={<AnalysisWorkspace />} />
          <Route path="/workflow-analysis" element={<WorkflowAnalysis />} />
          <Route path="/visualization" element={<SignalViewer />} />
          <Route path="/workflow" element={<WorkflowBuilder />} />
          <Route path="/settings" element={<Settings />} />
        </Routes>
      </AppShell>
    </div>
  )
}

export default App
